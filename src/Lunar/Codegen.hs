-- | The Tidal code for a song, written the way it would be typed into a
-- live-coding editor. "Lunar.Compile" builds its patterns from the same
-- pieces ('sourceMini', 'paramExpr'), so the code on screen is what plays.
module Lunar.Codegen
  ( Tok (..)
  , TokClass (..)
  , Line
  , songCode
  , songCodeTagged
  , songCodeText
  , trackCode
  , transformCode
  , sourceMini
  , sourceRule
  , ParamExpr (..)
  , paramExpr
  , paramCode
  , showNum
  , showRat
  , channelNumbers
  ) where

import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text qualified as T
import Lunar.Catalog (isPitched)
import Lunar.Model
import Lunar.Refactor
import Numeric (showFFloat)

-- | A piece of code with what it is, for highlighting.
data TokClass = TkPlain | TkFunc | TkString | TkNumber | TkOp | TkComment
  deriving (Eq, Show)

data Tok = Tok {tokClass :: !TokClass, tokText :: !Text}
  deriving (Eq, Show)

type Line = [Tok]

-- | A parameter as the code sets it: a constant, or a signal swept over a
-- range. Values are already rounded as they are printed.
data ParamExpr
  = PConst !Double
  | PRange !Double !Double !Signal !Rational
  deriving (Eq, Show)

-- | The mini-notation a track's source plays, and the rule that shortened a
-- step grid, if one did.
sourceMini :: Track -> Text
sourceMini t = case trackSource t of
  Steps xs -> rfText (refactorSteps pitched (trackSound t) xs)
  Euclid k n r v
    | n <= 0 || k <= 0 -> "~"
    | k >= n -> stepToken pitched (trackSound t) v <> "*" <> tshow n
    | otherwise ->
        stepToken pitched (trackSound t) v
          <> "(" <> tshow k <> "," <> tshow n <> (if r `mod` n == 0 then "" else "," <> tshow (r `mod` n)) <> ")"
  Mini txt -> if T.null (T.strip txt) then "~" else T.strip txt
  where
    pitched = isPitched (trackSound t)

sourceRule :: Track -> Maybe Text
sourceRule t = case trackSource t of
  Steps xs ->
    let r = refactorSteps (isPitched (trackSound t)) (trackSound t) xs
     in if rfRule r == "literal" then Nothing else Just (rfRule r)
  _ -> Nothing

-- | The expression a parameter's knob stands for, rounded as printed.
paramExpr :: Param -> ParamSetting -> ParamExpr
paramExpr p s = case psSignal s of
  SigNone -> PConst (roundFor p (psBase s))
  sig ->
    let (lo, hi) = sweep p s
     in PRange (roundFor p lo) (roundFor p hi) sig (psPeriod s)

-- | Where the sweep turns, @depth@ of the knob's travel either side of the
-- knob, in the knob's own (log for cutoff) scale.
sweep :: Param -> ParamSetting -> (Double, Double)
sweep p s =
  let (lo, hi) = paramRange p
      toN v
        | paramLog p = log (v / lo) / log (hi / lo)
        | otherwise = (v - lo) / (hi - lo)
      fromN x
        | paramLog p = lo * (hi / lo) ** x
        | otherwise = lo + x * (hi - lo)
      c = toN (psBase s)
      clamp01 = max 0 . min 1
   in (fromN (clamp01 (c - psDepth s)), fromN (clamp01 (c + psDepth s)))

roundFor :: Param -> Double -> Double
roundFor p v
  | p == Cutoff = fromIntegral (round v :: Int)
  | otherwise = fromIntegral (round (v * 100) :: Int) / 100

-- | A number as Tidal code writes it: @2@, @0.25@.
showNum :: Double -> Text
showNum v
  | v == fromIntegral r = tshow r
  | otherwise = T.pack (trimZeros (showFFloat (Just 4) v ""))
  where
    r = round v :: Int
    trimZeros s = reverse (dropWhile (== '.') (dropWhile (== '0') (reverse s)))

-- | A rational as code: @2@, @0.125@, or @(1/3)@ when it has no short decimal.
showRat :: Rational -> Text
showRat q
  | denominator q == 1 = tshow (numerator q)
  | terminating (denominator q) = showNum (fromRational q)
  | otherwise = "(" <> tshow (numerator q) <> "/" <> tshow (denominator q) <> ")"
  where
    terminating d = d `elem` [2, 4, 5, 8, 10, 16, 20, 25, 32, 40, 50, 64, 100, 125, 1000]

-- | A transform as the function it is in code: @every 4 (fast 2)@.
transformCode :: Transform -> [Tok]
transformCode = \case
  Fast r -> fn "fast" [num (showRat r)]
  Slow r -> fn "slow" [num (showRat r)]
  Rev -> [func "rev"]
  Palindrome -> [func "palindrome"]
  Brak -> [func "brak"]
  Iter n -> fn "iter" [num (tshow n)]
  Ply n -> fn "ply" [num (tshow n)]
  Rot n -> fn "rot" [num (tshow n)]
  Degrade p -> fn "degradeBy" [num (showNum p)]
  Chop n -> fn "chop" [num (tshow n)]
  Striate n -> fn "striate" [num (tshow n)]
  Hurry r -> fn "hurry" [num (showRat r)]
  Jux t -> fn "jux" [paren (transformCode t)]
  Every n t -> fn "every" [num (tshow n), paren (transformCode t)]
  Whenmod a b t -> fn "whenmod" [num (tshow a), num (tshow b), paren (transformCode t)]
  Sometimes t -> fn "sometimes" [paren (transformCode t)]
  Off r t -> fn "off" [num (showRat r), paren (transformCode t)]
  where
    fn name args = func name : concatMap (\a -> plain " " : a) args
    func = Tok TkFunc
    num t = [Tok TkNumber t]
    paren toks
      | length toks > 1 = plain "(" : toks <> [plain ")"]
      | otherwise = toks

paramCode :: Param -> ParamExpr -> [Tok]
paramCode p = \case
  PConst v -> [Tok TkOp "# ", Tok TkFunc (paramName p), plain " ", Tok TkNumber (showNum v)]
  PRange lo hi sig period ->
    [ Tok TkOp "# "
    , Tok TkFunc (paramName p)
    , plain " ("
    , Tok TkFunc "range"
    , plain " "
    , Tok TkNumber (showNum lo)
    , plain " "
    , Tok TkNumber (showNum hi)
    , Tok TkOp " $ "
    ]
      <> signalCode sig period
      <> [plain ")"]

signalCode :: Signal -> Rational -> [Tok]
signalCode sig period = case sig of
  SigRand -> [Tok TkFunc "rand"]
  _
    | period == 1 -> [Tok TkFunc name]
    | period > 1 -> [Tok TkFunc "slow", plain " ", Tok TkNumber (showRat period), plain " ", Tok TkFunc name]
    | otherwise -> [Tok TkFunc "fast", plain " ", Tok TkNumber (showRat (1 / period)), plain " ", Tok TkFunc name]
  where
    name = case sig of
      SigSine -> "sine"
      SigTri -> "tri"
      SigSaw -> "saw"
      SigSquare -> "square"
      SigPerlin -> "perlin"
      SigRand -> "rand"
      SigNone -> "sine"

plain :: Text -> Tok
plain = Tok TkPlain

-- | One track as a @dN $ ...@ block: transforms outermost first, then the
-- sound and its parameters. A muted track, or one another track's solo
-- silences, is commented out.
trackCode :: Bool -> Int -> Track -> [Line]
trackCode silenced channel t =
  map (if silenced then comment else id) $
    case chainLines of
      [] -> (lead <> base) : paramLines
      c : cs -> (lead <> c) : map (cont <>) cs <> [cont <> base] <> paramLines
  where
    lead = [Tok TkFunc ("d" <> tshow channel), Tok TkOp " $ "]
    cont = [plain "   ", Tok TkOp "$ "]
    chainLines = map (transformCode . snd) (trackChain t)
    mini = Tok TkString ("\"" <> sourceMini t <> "\"")
    pitched = isPitched (trackSound t)
    base
      | pitched =
          [Tok TkFunc "note", plain " ", mini, plain " ", Tok TkOp "# ", Tok TkFunc "s", plain " ", Tok TkString ("\"" <> trackSound t <> "\"")]
      | otherwise = [Tok TkFunc "s", plain " ", mini]
    paramLines =
      [ plain "   " : paramCode p (paramExpr p s)
      | (p, s) <- Map.toList (trackParams t)
      , paramActive p s
      ]
    comment line = Tok TkComment ("-- " <> T.concat (map tokText line)) : []

-- | The channel number each track plays on, @d1@ for the first.
channelNumbers :: Song -> [(Int, Track)]
channelNumbers = zip [1 ..] . songTracks

songCode :: Song -> [Line]
songCode = map snd . songCodeTagged

-- | The song's code, each line with the id of the track it belongs to.
songCodeTagged :: Song -> [(Maybe Int, Line)]
songCodeTagged song =
  (Nothing, [Tok TkFunc "setcps", plain " ", Tok TkNumber (showNum (fromIntegral (round (songCps song * 10000) :: Int) / 10000))])
    : concat
      [ (Nothing, []) : map ((,) (Just (trackId t))) (trackCode (silenced t) ch t)
      | (ch, t) <- channelNumbers song
      ]
  where
    anySolo = any trackSolo (songTracks song)
    silenced t = trackMuted t || (anySolo && not (trackSolo t))

songCodeText :: Song -> Text
songCodeText = T.unlines . map (T.concat . map tokText) . songCode

tshow :: Show a => a -> Text
tshow = T.pack . show
