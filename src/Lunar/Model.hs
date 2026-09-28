-- | The song as the sequencer edits it: tracks, each with a rhythm source, a
-- chain of pattern transforms, and parameters that may follow a continuous
-- signal. "Lunar.Compile" turns it into Tidal patterns and "Lunar.Codegen"
-- into the Tidal code that builds the same patterns.
module Lunar.Model
  ( Song (..)
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
  , demoSong
  , mapTrack
  , euclidSteps
  , resizeSteps
  , rotateSteps
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Sound.Tidal.Bjorklund (bjorklund)

data Song = Song
  { songCps :: !Double
  , songTracks :: ![Track]
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | Where a track's rhythm comes from. A step is 'Nothing' for a rest, or the
-- sample index (drums) or scale degree in semitones (pitched sounds).
data Source
  = Steps ![Maybe Int]
  | Euclid {euPulses :: !Int, euSteps :: !Int, euRotate :: !Int, euValue :: !Int}
  | Mini !Text
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Ord, Show, Enum, Bounded)

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
  deriving (Eq, Show, Enum, Bounded)

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
  deriving (Eq, Show)

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

nextTrackId :: Song -> Int
nextTrackId = (+ 1) . maximum . (0 :) . map trackId . songTracks

nextTransformId :: Track -> Int
nextTransformId = (+ 1) . maximum . (0 :) . map fst . trackChain

mapTrack :: Int -> (Track -> Track) -> Song -> Song
mapTrack tid f song = song {songTracks = map (\t -> if trackId t == tid then f t else t) (songTracks song)}

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

-- | The song LunarCycles opens with: a four-on-the-floor kick that doubles
-- every fourth cycle, a euclidean snare spread across the stereo field, hats
-- with a swept filter, and a bass line in mini-notation.
demoSong :: Song
demoSong =
  Song
    { songCps = 0.5625
    , songTracks =
        [ (newTrack 1 "kick" "bd" (Steps (concat (replicate 4 [Just 0, Nothing, Nothing, Nothing]))))
            { trackChain = [(1, Every 4 (Fast 2))]
            , trackParams = Map.fromList [(Gain, (defaultSetting Gain) {psBase = 1.1})]
            }
        , (newTrack 2 "snare" "sn" (Euclid 3 8 2 0))
            { trackChain = [(1, Jux Rev)]
            , trackParams = Map.fromList [(Room, (defaultSetting Room) {psBase = 0.25})]
            }
        , (newTrack 3 "hats" "hh" (Steps (concat (replicate 2 [Just 0, Nothing, Just 2, Nothing, Just 0, Nothing, Just 0, Just 1]))))
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
    }
