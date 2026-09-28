-- | Turning a step grid into mini-notation a person would write, and back.
--
-- 'refactorSteps' writes candidate spellings of a grid (@bd*4@ for four
-- evenly spaced kicks, @sn(3,8,2)@ for a rotated euclidean rhythm,
-- @[hh ~ hh:2 ~]*2@ for a repeated figure) and keeps the shortest one that
-- tidal-core itself plays the same as the literal grid. Nothing is taken on
-- trust: every candidate is parsed and queried before it is chosen.
module Lunar.Refactor
  ( Refactored (..)
  , refactorSteps
  , literalSteps
  , stepToken
  , miniToSteps
  ) where

import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ratio (denominator)
import Data.Text (Text)
import Data.Text qualified as T
import Lunar.Model (euclidSteps)
import Sound.Tidal.ParseBP (parseBP)
import Sound.Tidal.Pattern

-- | A spelling of a grid, and what made it shorter than the literal one.
data Refactored = Refactored
  { rfText :: !Text
  , rfRule :: !Text
  }
  deriving (Eq, Show)

-- | The token a step plays: @bd@, @bd:2@ for another sample of the same
-- folder, or a note number for a pitched sound.
stepToken :: Bool -> Text -> Int -> Text
stepToken pitched sound v
  | pitched = T.pack (show v)
  | v == 0 = sound
  | otherwise = sound <> ":" <> T.pack (show v)

-- | The grid spelled step by step, rests as @~@.
literalSteps :: Bool -> Text -> [Maybe Int] -> Text
literalSteps pitched sound steps
  | null steps = "~"
  | otherwise = T.unwords (map (maybe "~" (stepToken pitched sound)) steps)

-- | The simplest spelling of the grid that tidal-core plays the same:
-- shortest, with each bracket counting as three characters, so a spelling
-- nests only when that saves real typing. Drum
-- hits are compared by onset and sample, since a drum sample plays out
-- whatever its event length; pitched notes by onset, length and pitch.
refactorSteps :: Bool -> Text -> [Maybe Int] -> Refactored
refactorSteps pitched sound steps =
  case filter (sameAs . rfText) (sortOn (cost . rfText) candidates) of
    best : _ -> best
    [] -> Refactored literal "literal"
  where
    toks = map (fmap (stepToken pitched sound)) steps
    literal = literalSteps pitched sound steps
    reference = eventsOf literal
    sameAs txt = txt == literal || (isJust reference && eventsOf txt == reference)
    eventsOf txt = map key <$> queryText txt
    key e = (wholeStart e, if pitched then Just (wholeStop e) else Nothing, value e)
    candidates = Refactored literal "literal" : spellings (not pitched) toks
    cost t = T.length t + 3 * T.count "[" t

-- | Candidate spellings of a token sequence, each labelled with its rule,
-- in order of preference among spellings of the same length.
-- @reduce@ allows dropping resolution (@bd ~ ~ ~@ to @bd@), which changes
-- event lengths and so suits drums only.
spellings :: Bool -> [Maybe Text] -> [Refactored]
spellings reduce toks =
  nubOn rfText $
    concat
      [ [Refactored "~" "silence" | all isNothing toks, not (null toks)]
      , repeated
      , euclid
      , reduced
      , grouped
      , [Refactored (runLength toks) "repeats as !" | hasRun toks]
      ]
  where
    n = length toks
    hits = [t | Just t <- toks]
    reduced =
      [ Refactored (rfText r) ("coarser grid: " <> tshow (n `div` d) <> " steps")
      | reduce
      , d <- reverse (divisors n)
      , d > 1
      , and [isNothing t | (i, t) <- zip [0 :: Int ..] toks, i `mod` d /= 0]
      , let coarse = [t | (i, t) <- zip [0 :: Int ..] toks, i `mod` d == 0]
      , r <- take 1 (sortOn (T.length . rfText) (plain coarse : spellings reduce coarse))
      ]
    repeated =
      [ Refactored txt ("figure repeated " <> tshow reps <> "×")
      | u <- divisors n
      , u < n
      , let reps = n `div` u
            unit = take u toks
      , concat (replicate reps unit) == toks
      , let unitText = bestOf unit
            txt = if u == 1 then unitText <> "*" <> tshow reps else "[" <> unitText <> "]*" <> tshow reps
      , not (all isNothing unit)
      ]
    euclid =
      [ Refactored txt ("euclidean " <> args)
      | [t] <- [nub hits]
      , let k = length hits
      , k > 1 && k < n
      , r <- [0 .. n - 1]
      , map (fmap (const t)) (euclidSteps k n r 0) == toks
      , let args = "(" <> tshow k <> "," <> tshow n <> (if r == 0 then "" else "," <> tshow r) <> ")"
            txt = t <> args
      ]
    grouped =
      [ Refactored (T.unwords (map (bracket . bestOf) groups)) ("grouped in " <> tshow g)
      | g <- [2, 3, 4]
      , n `mod` g == 0
      , n `div` g > 1
      , let size = n `div` g
            groups = chunks size toks
      , any (all isNothing) groups || reduce
      ]
    bestOf xs = case sortOn T.length (map rfText (plain xs : spellings reduce xs)) of
      t : _ -> t
      [] -> plainText xs
    plain xs = Refactored (plainText xs) "literal"

plainText :: [Maybe Text] -> Text
plainText xs
  | null xs = "~"
  | otherwise = T.unwords (map (fromMaybe "~") xs)

-- | A spelling as one step of an enclosing sequence.
bracket :: Text -> Text
bracket t
  | T.any (== ' ') t = "[" <> t <> "]"
  | otherwise = t

-- | @bd bd bd@ as @bd!3@. A pair stays as it is, which reads better for the
-- one character @!2@ would save.
runLength :: [Maybe Text] -> Text
runLength = T.unwords . map spell . runs
  where
    spell (Just t, k) | k >= 3 = t <> "!" <> tshow k
    spell (t, k) = T.unwords (replicate k (fromMaybe "~" t))

hasRun :: [Maybe Text] -> Bool
hasRun = any (\(t, k) -> isJust t && k >= 3) . runs

runs :: Eq a => [a] -> [(a, Int)]
runs [] = []
runs (x : xs) = let (same, rest) = span (== x) xs in (x, 1 + length same) : runs rest

divisors :: Int -> [Int]
divisors n = [d | d <- [1 .. n], n `mod` d == 0]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks k xs = let (a, b) = splitAt k xs in a : chunks k b

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = go []
  where
    go _ [] = []
    go seen (x : xs)
      | f x `elem` seen = go seen xs
      | otherwise = x : go (f x : seen) xs

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Two cycles of a mini-notation string's events, sorted, or 'Nothing' when
-- it does not parse.
queryText :: Text -> Maybe [Event String]
queryText txt = case parseBP (T.unpack txt) of
  Left _ -> Nothing
  Right pat -> Just (sortOn fullKey (filter eventHasOnset (queryArc (pat :: Pattern String) (Arc 0 2))))

fullKey :: Event String -> (Time, Time, String)
fullKey e = (wholeStart e, wholeStop e, value e)

-- | Read mini-notation back into a grid, when it is one: a single sound (or
-- notes, for a pitched track) with at most one hit per step, the same every
-- cycle, on a grid of at most 32 steps. The result also names the sound the
-- hits use, for a drum track.
miniToSteps :: Bool -> Text -> Either Text (Text, [Maybe Int])
miniToSteps pitched txt = do
  pat <- parseStrings txt
  let cycleOf c = sortOn wholeStart (filter eventHasOnset (queryArc pat (Arc c (c + 1))))
      first = cycleOf 0
      shifted = map (\e -> (wholeStart e + 1, value e)) first
      second = map (\e -> (wholeStart e, value e)) (cycleOf 1)
  if shifted /= second
    then Left "changes from cycle to cycle"
    else pure ()
  let grid = foldr lcm 1 (map (fromIntegral . denominator . wholeStart) first) :: Int
      size = grid * ((8 + grid - 1) `div` grid)
  if grid > 32
    then Left "needs more than 32 steps"
    else pure ()
  let slots = Map.fromListWith (++) [(floor (wholeStart e * fromIntegral size) :: Int, [value e]) | e <- first]
  if any ((> 1) . length) (Map.elems slots)
    then Left "plays chords"
    else pure ()
  parsed <- traverse (parseToken pitched) (Map.mapMaybe listToMaybe slots)
  let sounds = nub (mapMaybe fst (Map.elems parsed))
  sound <- case sounds of
    [] -> pure ""
    [s] -> pure (T.pack s)
    _ -> Left "mixes sounds"
  pure (sound, [snd <$> Map.lookup i parsed | i <- [0 .. size - 1]])

parseStrings :: Text -> Either Text (Pattern String)
parseStrings txt = either (const (Left "does not parse")) Right (parseBP (T.unpack txt))

-- | A token's sound and step value: @bd:2@ is @(Just "bd", 2)@, a note
-- @7@ is @(Nothing, 7)@.
parseToken :: Bool -> String -> Either Text (Maybe String, Int)
parseToken pitched tok
  | pitched = case reads tok of
      [(v, "")] -> Right (Nothing, v)
      _ -> Left "notes must be whole numbers"
  | otherwise = case break (== ':') tok of
      (s, "") -> Right (Just s, 0)
      (s, ':' : rest) | [(v, "")] <- reads rest -> Right (Just s, v)
      _ -> Left "unreadable sample name"
