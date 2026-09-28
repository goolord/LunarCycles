-- | The song as the sequencer edits it: sequences of tracks, and a playlist
-- that places the sequences in time. Each track has a rhythm source, a chain
-- of pattern transforms, and parameters that may follow a continuous
-- signal. "Lunar.Compile" turns it into Tidal patterns and "Lunar.Codegen"
-- into the Tidal code that builds the same patterns.
module Lunar.Model
  ( Song (..)
  , Sequence (..)
  , Clip (..)
  , PlayMode (..)
  , Track (..)
  , Source (..)
  , SourceMode (..)
  , sourceMode
  , Transform (..)
  , TransformKind (..)
  , transformKind
  , defaultTransform
  , simpleKinds
  , chainKinds
  , Param (..)
  , allParams
  , ParamSetting (..)
  , Signal (..)
  , allSignals
  , paramDefault
  , paramRange
  , paramLog
  , paramName
  , paramActive
  , defaultSetting
  , newTrack
  , nextTrackId
  , nextTransformId
  , newSequence
  , nextSequenceId
  , nextClipId
  , findSequence
  , mapSequence
  , removeSequence
  , duplicateSequence
  , duplicateTrack
  , songLength
  , clipEnd
  , demoSong
  , blankSong
  , mapTrack
  , euclidSteps
  , resizeSteps
  , rotateSteps
  ) where

import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Sound.Tidal.Bjorklund (bjorklund)

data Song = Song
  { songCps :: !Double
  , songSequences :: ![Sequence]
    -- ^ Never empty: there is always a sequence to edit.
  , songPlaylist :: ![Clip]
  }
  deriving (Eq, Show, Read)

-- | Tracks that play together, such as a groove, a fill or a break. The
-- editor works on one sequence at a time, and the playlist chains them.
data Sequence = Sequence
  { seqId :: !Int
  , seqName :: !Text
  , seqCycles :: !Int
    -- ^ How many cycles a clip of the sequence covers when it is placed.
  , seqTracks :: ![Track]
  }
  deriving (Eq, Show, Read)

-- | A sequence placed in the playlist, on a lane, for whole cycles. The
-- sequence plays from its own cycle 0 at the clip's start, as Tidal's
-- @seqP@ plays it, and carries on past its length when the clip is longer.
data Clip = Clip
  { clipId :: !Int
  , clipSequence :: !Int
  , clipLane :: !Int
  , clipStart :: !Int
  , clipCycles :: !Int
  }
  deriving (Eq, Show, Read)

-- | What the transport plays: the sequence being edited, on a loop, or the
-- playlist from start to end.
data PlayMode = PlaySequence | PlaySong
  deriving (Eq, Show, Enum, Bounded)

data Track = Track
  { trackId :: !Int
  , trackName :: !Text
  , trackSound :: !Text
  , trackSource :: !Source
  , trackChain :: ![(Int, Transform)]
    -- ^ Outermost first, as the code reads: @every 4 (fast 2) $ jux rev $ ...@
    -- has @every@ at the head. Each transform carries an id that keeps its
    -- block's widget state while blocks are dragged past each other.
  , trackParams :: !(Map Param ParamSetting)
  , trackMuted :: !Bool
  , trackSolo :: !Bool
  }
  deriving (Eq, Show, Read)

-- | Where a track's rhythm comes from. A step is 'Nothing' for a rest, or the
-- sample index (drums) or scale degree in semitones (pitched sounds).
data Source
  = Steps ![Maybe Int]
  | Euclid {euPulses :: !Int, euSteps :: !Int, euRotate :: !Int, euValue :: !Int}
  | Mini !Text
  deriving (Eq, Show, Read)

data SourceMode = ModeSteps | ModeEuclid | ModeMini
  deriving (Eq, Show, Enum, Bounded)

sourceMode :: Source -> SourceMode
sourceMode = \case
  Steps _ -> ModeSteps
  Euclid {} -> ModeEuclid
  Mini _ -> ModeMini

data Transform
  = Fast !Rational
  | Slow !Rational
  | Rev
  | Palindrome
  | Brak
  | Iter !Int
  | Ply !Int
  | Rot !Int
  | Degrade !Double
  | Chop !Int
  | Striate !Int
  | Hurry !Rational
  | Jux !Transform
  | Every !Int !Transform
  | Whenmod !Int !Int !Transform
  | Sometimes !Transform
  | Off !Rational !Transform
  deriving (Eq, Show, Read)

data TransformKind
  = KFast | KSlow | KRev | KPalindrome | KBrak | KIter | KPly | KRot | KDegrade
  | KChop | KStriate | KHurry | KJux | KEvery | KWhenmod | KSometimes | KOff
  deriving (Eq, Show, Enum, Bounded)

transformKind :: Transform -> TransformKind
transformKind = \case
  Fast _ -> KFast
  Slow _ -> KSlow
  Rev -> KRev
  Palindrome -> KPalindrome
  Brak -> KBrak
  Iter _ -> KIter
  Ply _ -> KPly
  Rot _ -> KRot
  Degrade _ -> KDegrade
  Chop _ -> KChop
  Striate _ -> KStriate
  Hurry _ -> KHurry
  Jux _ -> KJux
  Every _ _ -> KEvery
  Whenmod {} -> KWhenmod
  Sometimes _ -> KSometimes
  Off _ _ -> KOff

-- | The transform a new block of this kind starts as.
defaultTransform :: TransformKind -> Transform
defaultTransform = \case
  KFast -> Fast 2
  KSlow -> Slow 2
  KRev -> Rev
  KPalindrome -> Palindrome
  KBrak -> Brak
  KIter -> Iter 4
  KPly -> Ply 2
  KRot -> Rot 1
  KDegrade -> Degrade 0.3
  KChop -> Chop 4
  KStriate -> Striate 4
  KHurry -> Hurry 2
  KJux -> Jux Rev
  KEvery -> Every 4 (Fast 2)
  KWhenmod -> Whenmod 8 6 Rev
  KSometimes -> Sometimes (Ply 2)
  KOff -> Off 0.125 (Fast 2)

-- | Kinds that can sit inside a higher-order block (@every 4 (…)@).
simpleKinds :: [TransformKind]
simpleKinds = [KFast, KSlow, KRev, KPalindrome, KBrak, KIter, KPly, KRot, KDegrade, KChop, KHurry]

-- | Kinds offered by a track's "add" menu.
chainKinds :: [TransformKind]
chainKinds = [minBound .. maxBound]

-- | Continuous controls a knob sets, in SuperDirt's names.
data Param = Cutoff | Resonance | Gain | Pan | Speed | Begin | End | Room | Shape
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

allParams :: [Param]
allParams = [minBound .. maxBound]

paramName :: Param -> Text
paramName = \case
  Cutoff -> "cutoff"
  Resonance -> "resonance"
  Gain -> "gain"
  Pan -> "pan"
  Speed -> "speed"
  Begin -> "begin"
  End -> "end"
  Room -> "room"
  Shape -> "shape"

-- | SuperDirt's value when the parameter is left out.
paramDefault :: Param -> Double
paramDefault = \case
  Cutoff -> 20000
  Resonance -> 0
  Gain -> 1
  Pan -> 0.5
  Speed -> 1
  Begin -> 0
  End -> 1
  Room -> 0
  Shape -> 0

paramRange :: Param -> (Double, Double)
paramRange = \case
  Cutoff -> (50, 20000)
  Resonance -> (0, 0.9)
  Gain -> (0, 1.5)
  Pan -> (0, 1)
  Speed -> (-2, 4)
  Begin -> (0, 1)
  End -> (0, 1)
  Room -> (0, 1)
  Shape -> (0, 0.95)

-- | Whether the knob moves through the range on a log scale.
paramLog :: Param -> Bool
paramLog = (== Cutoff)

-- | Which shape a parameter's modulation follows.
data Signal = SigNone | SigSine | SigTri | SigSaw | SigSquare | SigRand | SigPerlin
  deriving (Eq, Show, Read, Enum, Bounded)

allSignals :: [Signal]
allSignals = [minBound .. maxBound]

-- | A knob's position, and the signal that sweeps around it. @depth@ is the
-- fraction of the knob's travel the sweep covers either side of @base@, and
-- the sweep takes @period@ cycles.
data ParamSetting = ParamSetting
  { psBase :: !Double
  , psSignal :: !Signal
  , psDepth :: !Double
  , psPeriod :: !Rational
  }
  deriving (Eq, Show, Read)

defaultSetting :: Param -> ParamSetting
defaultSetting p = ParamSetting (paramDefault p) SigNone 0.3 4

-- | Whether the setting changes the sound, and so belongs in the code.
paramActive :: Param -> ParamSetting -> Bool
paramActive p s = psSignal s /= SigNone || abs (psBase s - paramDefault p) > 1e-6

newTrack :: Int -> Text -> Text -> Source -> Track
newTrack tid name sound src =
  Track
    { trackId = tid
    , trackName = name
    , trackSound = sound
    , trackSource = src
    , trackChain = []
    , trackParams = Map.empty
    , trackMuted = False
    , trackSolo = False
    }

nextTrackId :: Sequence -> Int
nextTrackId = (+ 1) . maximum . (0 :) . map trackId . seqTracks

nextTransformId :: Track -> Int
nextTransformId = (+ 1) . maximum . (0 :) . map fst . trackChain

mapTrack :: Int -> (Track -> Track) -> Sequence -> Sequence
mapTrack tid f sq = sq {seqTracks = map (\t -> if trackId t == tid then f t else t) (seqTracks sq)}

-- | A copy of a track placed after it, with a fresh id.
duplicateTrack :: Int -> Sequence -> (Sequence, Maybe Int)
duplicateTrack tid sq = case break ((== tid) . trackId) (seqTracks sq) of
  (before, t : after) ->
    let fresh = nextTrackId sq
     in (sq {seqTracks = before <> [t, t {trackId = fresh, trackName = trackName t <> " copy", trackSolo = False}] <> after}, Just fresh)
  _ -> (sq, Nothing)

newSequence :: Int -> Text -> Sequence
newSequence sid name = Sequence {seqId = sid, seqName = name, seqCycles = 4, seqTracks = []}

nextSequenceId :: Song -> Int
nextSequenceId = (+ 1) . maximum . (0 :) . map seqId . songSequences

nextClipId :: Song -> Int
nextClipId = (+ 1) . maximum . (0 :) . map clipId . songPlaylist

findSequence :: Int -> Song -> Maybe Sequence
findSequence sid = find ((== sid) . seqId) . songSequences

mapSequence :: Int -> (Sequence -> Sequence) -> Song -> Song
mapSequence sid f song = song {songSequences = map (\sq -> if seqId sq == sid then f sq else sq) (songSequences song)}

-- | Remove a sequence and its clips, unless it is the only one.
removeSequence :: Int -> Song -> Song
removeSequence sid song
  | length (songSequences song) <= 1 = song
  | otherwise =
      song
        { songSequences = filter ((/= sid) . seqId) (songSequences song)
        , songPlaylist = filter ((/= sid) . clipSequence) (songPlaylist song)
        }

-- | A copy of a sequence placed after it, and the copy's id.
duplicateSequence :: Int -> Song -> (Song, Maybe Int)
duplicateSequence sid song = case break ((== sid) . seqId) (songSequences song) of
  (before, sq : after) ->
    let fresh = nextSequenceId song
        copy = sq {seqId = fresh, seqName = seqName sq <> " copy"}
     in (song {songSequences = before <> [sq, copy] <> after}, Just fresh)
  _ -> (song, Nothing)

clipEnd :: Clip -> Int
clipEnd c = clipStart c + clipCycles c

-- | Where the playlist ends: the end of its last clip, in cycles.
songLength :: Song -> Int
songLength = maximum . (0 :) . map clipEnd . songPlaylist

-- | The steps of a euclidean rhythm as the step grid would show them.
euclidSteps :: Int -> Int -> Int -> Int -> [Maybe Int]
euclidSteps k n r v
  | n <= 0 = []
  | otherwise =
      let base = bjorklund (max 0 (min k n), n)
          rotL = r `mod` n
       in [if b then Just v else Nothing | b <- drop rotL base ++ take rotL base]

-- | Change the step count, keeping each hit where it falls in the cycle when
-- the counts divide evenly and keeping the leading steps otherwise.
resizeSteps :: Int -> [Maybe Int] -> [Maybe Int]
resizeSteps n xs
  | n <= 0 = []
  | old == 0 = replicate n Nothing
  | n `mod` old == 0 = concatMap (\x -> x : replicate (n `div` old - 1) Nothing) xs
  | old `mod` n == 0 && all (\(i, x) -> i `mod` (old `div` n) == 0 || x == Nothing) (zip [0 ..] xs) =
      [x | (i, x) <- zip [0 :: Int ..] xs, i `mod` (old `div` n) == 0]
  | otherwise = take n (xs ++ repeat Nothing)
  where
    old = length xs

rotateSteps :: Int -> [a] -> [a]
rotateSteps _ [] = []
rotateSteps k xs = let m = k `mod` length xs in drop m xs ++ take m xs

-- | A new song: one sequence, holding an empty kick grid to start from.
blankSong :: Song
blankSong =
  Song
    { songCps = 0.5625
    , songSequences = [(newSequence 1 "sequence 1") {seqTracks = [newTrack 1 "kick" "bd" (Steps (replicate 16 Nothing))]}]
    , songPlaylist = []
    }

-- | The song LunarCycles opens with. Its groove is a four-on-the-floor
-- kick that doubles every fourth cycle, a euclidean snare spread across the
-- stereo field, hats with a swept filter, and a bass line in mini-notation.
-- An intro of hats and a filtered bass leads into it, and a two-cycle break
-- splits it in two and fills its last two cycles.
demoSong :: Song
demoSong =
  Song
    { songCps = 0.5625
    , songSequences =
        [ Sequence 1 "groove" 4
            [ (newTrack 1 "kick" "bd" (Steps (concat (replicate 4 [Just 0, Nothing, Nothing, Nothing]))))
                { trackChain = [(1, Every 4 (Fast 2))]
                , trackParams = Map.fromList [(Gain, (defaultSetting Gain) {psBase = 1.1})]
                }
            , (newTrack 2 "snare" "sn" (Euclid 3 8 2 0))
                { trackChain = [(1, Jux Rev)]
                , trackParams = Map.fromList [(Room, (defaultSetting Room) {psBase = 0.25})]
                }
            , hats
                { trackChain = [(1, Degrade 0.25), (2, Every 3 Rev)]
                , trackParams =
                    Map.fromList
                      [ (Cutoff, ParamSetting 3000 SigSine 0.35 4)
                      , (Gain, (defaultSetting Gain) {psBase = 0.85})
                      , (Pan, ParamSetting 0.5 SigTri 0.4 2)
                      ]
                }
            , (newTrack 4 "bass" "superpiano" (Mini "0 [~ 0] <3 5> [7 ~ 12 ~]"))
                { trackChain = [(1, Off 0.125 (Fast 2))]
                , trackParams = Map.fromList [(Cutoff, ParamSetting 900 SigSaw 0.25 8)]
                }
            ]
        , Sequence 2 "intro" 4
            [ hats
                { trackChain = [(1, Every 3 Rev)]
                , trackParams = Map.fromList [(Cutoff, ParamSetting 1200 SigSaw 0.3 4), (Gain, (defaultSetting Gain) {psBase = 0.8})]
                }
            , (newTrack 4 "bass" "superpiano" (Mini "0 ~ ~ 0 ~ ~ <3 5> ~"))
                { trackParams = Map.fromList [(Cutoff, (defaultSetting Cutoff) {psBase = 500})]
                }
            ]
        , Sequence 3 "break" 2
            [ newTrack 1 "kick" "bd" (Steps [Just 0, Nothing, Nothing, Nothing, Nothing, Nothing, Just 0, Nothing])
            , (newTrack 2 "snare" "sn" (Euclid 5 8 0 0))
                { trackChain = [(1, Sometimes (Ply 2))]
                }
            , newTrack 3 "clap" "cp" (Steps [Nothing, Just 0, Nothing, Just 0])
            ]
        ]
    , songPlaylist =
        [ Clip 1 2 0 0 4
        , Clip 2 1 0 4 8
        , Clip 3 3 0 12 2
        , Clip 4 1 0 14 10
        , Clip 5 3 1 22 2
        ]
    }
  where
    hats = newTrack 3 "hats" "hh" (Steps (concat (replicate 2 [Just 0, Nothing, Just 2, Nothing, Just 0, Nothing, Just 0, Just 1])))
