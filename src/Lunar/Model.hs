-- | The song as the sequencer edits it: channels, the sequences that give
-- each channel a part, and a playlist that places the sequences in time. A
-- channel is an instrument, its sound and parameters (which may follow a
-- continuous signal), shared by every sequence; its part in a sequence is a
-- rhythm source and a chain of pattern transforms. "Lunar.Compile" turns it into Tidal patterns and "Lunar.Codegen"
-- into the Tidal code that builds the same patterns.
module Lunar.Model
  ( Song (..)
  , Channel (..)
  , Sequence (..)
  , Part (..)
  , emptyPart
  , Clip (..)
  , PlayMode (..)
  , Track (..)
  , channelTrack
  , sequenceTracks
  , partsIn
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
  , newChannel
  , nextChannelId
  , addChannel
  , removeChannel
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
  , songChannels :: ![Channel]
    -- ^ In the order the track list shows them; the first plays on @d1@.
  , songSequences :: ![Sequence]
    -- ^ Never empty: there is always a sequence to edit.
  , songPlaylist :: ![Clip]
  }
  deriving (Eq, Show, Read)

-- | An instrument of the song: what it plays through and how it is heard.
-- Every sequence has every channel, so changing a channel's sound changes
-- it wherever the channel plays.
data Channel = Channel
  { chanId :: !Int
  , chanName :: !Text
  , chanSound :: !Text
  , chanParams :: !(Map Param ParamSetting)
  , chanMuted :: !Bool
  , chanSolo :: !Bool
  }
  deriving (Eq, Show, Read)

-- | Parts that play together, such as a groove, a fill or a break. The
-- editor works on one sequence at a time, and the playlist chains them.
data Sequence = Sequence
  { seqId :: !Int
  , seqName :: !Text
  , seqCycles :: !Int
    -- ^ How many cycles a clip of the sequence covers when it is placed.
  , seqParts :: !(Map Int Part)
    -- ^ Each channel's part, by channel id. A channel without one is
    -- silent in the sequence.
  }
  deriving (Eq, Show, Read)

-- | What a channel plays in one sequence.
data Part = Part
  { partSource :: !Source
  , partChain :: ![(Int, Transform)]
    -- ^ Outermost first, as the code reads: @every 4 (fast 2) $ jux rev $ ...@
    -- has @every@ at the head. Each transform carries an id that keeps its
    -- block's widget state while blocks are dragged past each other.
  }
  deriving (Eq, Show, Read)

-- | What the editor shows for a channel that has no part in a sequence.
emptyPart :: Part
emptyPart = Part (Steps (replicate 16 Nothing)) []

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

-- | A channel as one sequence plays it: the channel's settings with its part
-- there. It is what the editors, compiler and code work on; 'mapTrack'
-- writes a changed one back to the channel and the part.
data Track = Track
  { trackId :: !Int
    -- ^ The channel's id.
  , trackName :: !Text
  , trackSound :: !Text
  , trackSource :: !Source
  , trackChain :: ![(Int, Transform)]
  , trackParams :: !(Map Param ParamSetting)
  , trackMuted :: !Bool
  , trackSolo :: !Bool
  }
  deriving (Eq, Show)

channelTrack :: Sequence -> Channel -> Track
channelTrack sq ch =
  Track
    { trackId = chanId ch
    , trackName = chanName ch
    , trackSound = chanSound ch
    , trackSource = partSource part
    , trackChain = partChain part
    , trackParams = chanParams ch
    , trackMuted = chanMuted ch
    , trackSolo = chanSolo ch
    }
  where
    part = Map.findWithDefault emptyPart (chanId ch) (seqParts sq)

-- | Every channel as the sequence plays it, in channel order.
sequenceTracks :: Song -> Sequence -> [Track]
sequenceTracks song sq = map (channelTrack sq) (songChannels song)

-- | Whether the channel has a part in the sequence.
partsIn :: Sequence -> Int -> Bool
partsIn sq cid = Map.member cid (seqParts sq)

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

newChannel :: Int -> Text -> Text -> Channel
newChannel cid name sound =
  Channel
    { chanId = cid
    , chanName = name
    , chanSound = sound
    , chanParams = Map.empty
    , chanMuted = False
    , chanSolo = False
    }

nextChannelId :: Song -> Int
nextChannelId = (+ 1) . maximum . (0 :) . map chanId . songChannels

nextTransformId :: Track -> Int
nextTransformId = (+ 1) . maximum . (0 :) . map fst . trackChain

-- | Add a channel after the others, with a part in one sequence.
addChannel :: Int -> Channel -> Part -> Song -> Song
addChannel sid ch part song =
  mapSequence sid (\sq -> sq {seqParts = Map.insert (chanId ch) part (seqParts sq)}) song {songChannels = songChannels song <> [ch]}

-- | Remove a channel, and its part from every sequence.
removeChannel :: Int -> Song -> Song
removeChannel cid song =
  song
    { songChannels = filter ((/= cid) . chanId) (songChannels song)
    , songSequences = [sq {seqParts = Map.delete cid (seqParts sq)} | sq <- songSequences song]
    }

-- | Change a channel as a sequence plays it. The channel's settings change
-- in every sequence; the part changes only in this one, and a channel
-- without a part here gets one only when the part itself changes.
mapTrack :: Int -> Int -> (Track -> Track) -> Song -> Song
mapTrack sid cid f song = case (findSequence sid song, find ((== cid) . chanId) (songChannels song)) of
  (Just sq, Just ch) ->
    let t = f (channelTrack sq ch)
        shown = Map.findWithDefault emptyPart cid (seqParts sq)
        part = Part (trackSource t) (trackChain t)
        ch' = ch {chanName = trackName t, chanSound = trackSound t, chanParams = trackParams t, chanMuted = trackMuted t, chanSolo = trackSolo t}
        song' = song {songChannels = map (\c -> if chanId c == cid then ch' else c) (songChannels song)}
     in if part == shown then song' else mapSequence sid (\s -> s {seqParts = Map.insert cid part (seqParts s)}) song'
  _ -> song

-- | A copy of a channel placed after it, with a fresh id, playing the
-- original's part in this sequence only.
duplicateTrack :: Int -> Int -> Song -> (Song, Maybe Int)
duplicateTrack sid cid song = case break ((== cid) . chanId) (songChannels song) of
  (before, ch : after) ->
    let fresh = nextChannelId song
        copy = ch {chanId = fresh, chanName = chanName ch <> " copy", chanSolo = False}
        copyPart sq = sq {seqParts = maybe id (Map.insert fresh) (Map.lookup cid (seqParts sq)) (seqParts sq)}
     in (mapSequence sid copyPart song {songChannels = before <> [ch, copy] <> after}, Just fresh)
  _ -> (song, Nothing)

newSequence :: Int -> Text -> Sequence
newSequence sid name = Sequence {seqId = sid, seqName = name, seqCycles = 4, seqParts = Map.empty}

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
    , songChannels = [newChannel 1 "kick" "bd"]
    , songSequences = [(newSequence 1 "sequence 1") {seqParts = Map.fromList [(1, emptyPart)]}]
    , songPlaylist = []
    }

-- | The song LunarCycles opens with. Its groove is a four-on-the-floor
-- kick that doubles every fourth cycle, a euclidean snare spread across the
-- stereo field, hats with a swept filter, and a bass line in mini-notation.
-- An intro of hats and a sparser bass leads into it, and a two-cycle break
-- splits it in two and fills its last two cycles.
demoSong :: Song
demoSong =
  Song
    { songCps = 0.5625
    , songChannels =
        [ (newChannel 1 "kick" "bd") {chanParams = Map.fromList [(Gain, (defaultSetting Gain) {psBase = 1.1})]}
        , (newChannel 2 "snare" "sn") {chanParams = Map.fromList [(Room, (defaultSetting Room) {psBase = 0.25})]}
        , (newChannel 3 "hats" "hh")
            { chanParams =
                Map.fromList
                  [ (Cutoff, ParamSetting 3000 SigSine 0.35 4)
                  , (Gain, (defaultSetting Gain) {psBase = 0.85})
                  , (Pan, ParamSetting 0.5 SigTri 0.4 2)
                  ]
            }
        , (newChannel 4 "bass" "superpiano") {chanParams = Map.fromList [(Cutoff, ParamSetting 900 SigSaw 0.25 8)]}
        , newChannel 5 "clap" "cp"
        ]
    , songSequences =
        [ Sequence 1 "groove" 4 $
            Map.fromList
              [ (1, Part (Steps (concat (replicate 4 [Just 0, Nothing, Nothing, Nothing]))) [(1, Every 4 (Fast 2))])
              , (2, Part (Euclid 3 8 2 0) [(1, Jux Rev)])
              , (3, Part hats [(1, Degrade 0.25), (2, Every 3 Rev)])
              , (4, Part (Mini "0 [~ 0] <3 5> [7 ~ 12 ~]") [(1, Off 0.125 (Fast 2))])
              ]
        , Sequence 2 "intro" 4 $
            Map.fromList
              [ (3, Part hats [(1, Every 3 Rev)])
              , (4, Part (Mini "0 ~ ~ 0 ~ ~ <3 5> ~") [])
              ]
        , Sequence 3 "break" 2 $
            Map.fromList
              [ (1, Part (Steps [Just 0, Nothing, Nothing, Nothing, Nothing, Nothing, Just 0, Nothing]) [])
              , (2, Part (Euclid 5 8 0 0) [(1, Sometimes (Ply 2))])
              , (5, Part (Steps [Nothing, Just 0, Nothing, Just 0]) [])
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
    hats = Steps (concat (replicate 2 [Just 0, Nothing, Just 2, Nothing, Just 0, Nothing, Just 0, Just 1]))
