-- | The Tidal code for a song, written the way it would be typed into a
-- live-coding editor. "Lunar.Compile" builds its patterns from the same
-- pieces ('sourceMini', 'paramExpr'), so the code on screen is what plays.
module Lunar.Codegen
  ( Tok (..)
  , TokClass (..)
  , Line
  , CodeLines
  , songCode
  , sequenceCode
  , arrangementCode
  , codeText
  , bindingNames
  , trackCode
  , transformCode
  , sourceMini
  , sourceRule
  , ParamExpr (..)
  , paramExpr
  , paramCode
  , showNum
  , showRat
  ) where

import Data.Char (isAlphaNum, isAsciiLower, toLower)
import Data.List (sortOn)
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

-- | Code lines, each with the id of the track it belongs to, if any.
type CodeLines = [(Maybe Int, Line)]

-- | A track's expression over lines: its transforms outermost first, then
-- the sound, then a line for each parameter. Lines after the first start
-- with the @$@ or @#@ that joins them, and the caller indents them.
trackLines :: Track -> [Line]
trackLines t = case map (transformCode . snd) (trackChain t) of
  [] -> base : paramLines
  c : cs -> c : map (dollar <>) cs <> [dollar <> base] <> paramLines
  where
    dollar = [Tok TkOp "$ "]
    mini = Tok TkString ("\"" <> sourceMini t <> "\"")
    base
      | isPitched (trackSound t) =
          [Tok TkFunc "note", plain " ", mini, plain " ", Tok TkOp "# ", Tok TkFunc "s", plain " ", Tok TkString ("\"" <> trackSound t <> "\"")]
      | otherwise = [Tok TkFunc "s", plain " ", mini]
    paramLines = [paramCode p (paramExpr p s) | (p, s) <- Map.toList (trackParams t), paramActive p s]

-- | One track as a @dN $ ...@ block. A muted track, or one another track's
-- solo silences, is commented out.
trackCode :: Bool -> Int -> Track -> [Line]
trackCode silenced channel t =
  map (if silenced then comment else id) (zipWith (<>) (lead : repeat [plain "   "]) (trackLines t))
  where
    lead = [Tok TkFunc ("d" <> tshow channel), Tok TkOp " $ "]

comment :: Line -> Line
comment line = [Tok TkComment ("-- " <> T.concat (map tokText line))]

-- | Tracks that sound: all but the muted ones, or only the soloed ones.
silencedIn :: [Track] -> Track -> Bool
silencedIn tracks t = trackMuted t || (any trackSolo tracks && not (trackSolo t))

setcpsLine :: Song -> (Maybe Int, Line)
setcpsLine song = (Nothing, [Tok TkFunc "setcps", plain " ", Tok TkNumber (showNum (fromIntegral (round (songCps song * 10000) :: Int) / 10000))])

-- | The code for what the transport plays: the sequence being edited, or
-- the playlist. Lines of the edited sequence's tracks carry their ids.
songCode :: PlayMode -> Int -> Song -> CodeLines
songCode mode current song = case mode of
  PlaySequence -> sequenceCode song (findSequence current song)
  PlaySong -> arrangementCode current song

-- | A sequence as live code: a @dN@ for each channel with a part in it,
-- numbered by the channel's place in the song, so a channel keeps its
-- number in every sequence.
sequenceCode :: Song -> Maybe Sequence -> CodeLines
sequenceCode song msq =
  setcpsLine song
    : concat
      [ (Nothing, []) : map ((,) (Just (trackId t))) (trackCode (silencedIn tracks t) ch t)
      | Just sq <- [msq]
      , (ch, t) <- zip [1 ..] tracks
      , partsIn sq (trackId t)
      ]
  where
    tracks = maybe [] (sequenceTracks song) msq

-- | The playlist as live code: each placed sequence bound to its name as a
-- @stack@ of its tracks, and one channel that plays them where the
-- playlist puts them, looping at the playlist's end.
--
-- > let groove = stack
-- >       [ s "bd*4"
-- >       , s "~ sn"
-- >       ]
-- > d1 $ timeLoop 8 $ seqP
-- >    [ (0, 8, groove)
-- >    ]
arrangementCode :: Int -> Song -> CodeLines
arrangementCode current song
  | null clips =
      [ setcpsLine song
      , (Nothing, [])
      , (Nothing, [Tok TkComment "-- The playlist is empty. Place a sequence on it to hear the song."])
      , (Nothing, [Tok TkFunc "d1", Tok TkOp " $ ", Tok TkFunc "silence"])
      ]
  | otherwise =
      [setcpsLine song, (Nothing, [])]
        <> concat (zipWith binding [0 :: Int ..] placed)
        <> [(Nothing, [])]
        <> [(Nothing, [Tok TkFunc "d1", Tok TkOp " $ ", Tok TkFunc "timeLoop", plain " ", Tok TkNumber (tshow (songLength song)), Tok TkOp " $ ", Tok TkFunc "seqP"])]
        <> zipWith clipLine [0 :: Int ..] clips
        <> [(Nothing, [plain "   ]"])]
  where
    clips = sortOn (\c -> (clipStart c, clipLane c)) (songPlaylist song)
    names = bindingNames song
    nameOf sid = Map.findWithDefault "silence" sid names
    placed = [sq | sq <- songSequences song, any ((== seqId sq) . clipSequence) clips]
    clipLine i c =
      ( Nothing
      , [ plain (if i == 0 then "   [ (" else "   , (")
        , Tok TkNumber (tshow (clipStart c))
        , plain ", "
        , Tok TkNumber (tshow (clipEnd c))
        , plain (", " <> nameOf (clipSequence c) <> ")")
        ]
      )
    binding i sq =
      let everyTrack = sequenceTracks song sq
          tracks = filter (partsIn sq . trackId) everyTrack
          silenced = silencedIn everyTrack
          tag t = if seqId sq == current then Just (trackId t) else Nothing
          lead = plain ((if i == 0 then "let " else "    ") <> nameOf (seqId sq) <> " = ")
          audibleIds = [trackId t | t <- tracks, not (silenced t)]
          element t
            | silenced t = [(tag t, plain "      " : comment l) | l <- trackLines t]
            | otherwise =
                let sep = if take 1 audibleIds == [trackId t] then "[ " else ", "
                 in zipWith (\p l -> (tag t, plain p : l)) (("      " <> sep) : repeat "        ") (trackLines t)
          body = concatMap element tracks
       in if null audibleIds
            then (Nothing, [lead, Tok TkFunc "stack", plain " []"]) : body
            else (Nothing, [lead, Tok TkFunc "stack"]) : body <> [(Nothing, [plain "      ]"])]

-- | The name each sequence is bound to in the playlist's code: its own name
-- as a Haskell identifier, kept clear of keywords, of the functions the
-- code calls, and of the other sequences' names.
bindingNames :: Song -> Map.Map Int Text
bindingNames song = snd (foldl pick ([], Map.empty) (songSequences song))
  where
    pick (taken, acc) sq =
      let base = identifier (seqName sq)
          candidates = base : [base <> "_" <> tshow (seqId sq)] <> [base <> "_" <> tshow n | n <- [2 :: Int ..]]
          name = case [c | c <- candidates, c `notElem` taken, c `notElem` reserved] of
            c : _ -> c
            [] -> base
       in (name : taken, Map.insert (seqId sq) name acc)
    identifier txt =
      let cleaned = T.map (\ch -> if isAlphaNum ch || ch == '_' then toLower ch else '_') (T.strip txt)
          trimmed = T.dropWhile (== '_') cleaned
       in case T.uncons trimmed of
            Nothing -> "part"
            Just (ch, _)
              | isAsciiLower ch -> trimmed
              | otherwise -> "part_" <> trimmed
    reserved =
      [ "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix"
      , "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"
      , "stack", "seqP", "timeLoop", "silence", "s", "n", "note", "d1", "setcps", "range", "slow", "fast"
      , "sine", "tri", "saw", "square", "rand", "perlin", "rev", "every", "jux", "off", "ply", "iter"
      , "rot", "chop", "striate", "hurry", "brak", "palindrome", "degradeBy", "sometimes", "whenmod"
      , "cutoff", "resonance", "gain", "pan", "speed", "begin", "end", "room", "shape", "hush"
      ]

codeText :: CodeLines -> Text
codeText = T.unlines . map (T.concat . map tokText . snd)

tshow :: Show a => a -> Text
tshow = T.pack . show
