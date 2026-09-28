-- | Tidal events as MIDI. Live playback sends each note through Euterpea's
-- MIDI output; 'exportMidi' writes a stretch of the song as a Euterpea
-- 'Music' value and lets Euterpea render the file.
module Lunar.Midi
  ( MidiNote (..)
  , eventNote
  , trackChannel
  , songMusic
  , exportMidi
  ) where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Euterpea.IO.MIDI.GeneralMidi (toGM)
import Euterpea.IO.MIDI.ToMidi (writeMidi)
import Euterpea.Music
  ( InstrumentName (..)
  , Music (..)
  , Music1
  , NoteAttribute (Volume)
  , instrument
  , note
  , pitch
  , rest
  , tempo
  )
import Lunar.Catalog
import Lunar.Compile
import Sound.Tidal.Pattern (Event, EventF (..), ValueMap, eventHasOnset, wholeStart, wholeStop)

-- | One note as a MIDI device hears it.
data MidiNote = MidiNote
  { mnChannel :: !Int
  , mnKey :: !Int
  , mnVelocity :: !Int
  , mnSeconds :: !Double
  , mnProgram :: !(Maybe Int)
    -- ^ The General MIDI program a melodic channel needs.
  , mnInstrument :: !InstrumentName
  , mnControls :: ![(Int, Int)]
    -- ^ Controller values sent before the note: pan (10), brightness (74),
    -- resonance (71) and reverb send (91).
  }
  deriving (Show)

-- | The channel a track's melodic notes use. Channel 10 (index 9) is the
-- General MIDI drum kit and is left to drum sounds.
trackChannel :: Int -> Int
trackChannel channel = ([0 .. 8] <> [10 .. 15]) !! ((channel - 1) `mod` 15)

-- | The MIDI note for a Tidal event, if its sound is in the catalog. Event
-- length becomes note length at the given tempo; @gain@ sets velocity.
eventNote :: Double -> Int -> Event ValueMap -> Maybe MidiNote
eventNote cps channel e = do
  name <- valueText <$> Map.lookup "s" vm
  snd' <- lookupSound name
  let gain = num "gain" 1
      vel = clampI 1 127 (round (100 * gain ** 1.3))
      secs = max 0.03 (fromRational (wholeStop e - wholeStart e) / cps)
      controls =
        [(10, clampI 0 127 (round (num "pan" 0.5 * 127)))]
          <> [(74, cutoffCC c) | Just c <- [lookupNum "cutoff"]]
          <> [(71, clampI 0 127 (round (r / 0.9 * 127))) | Just r <- [lookupNum "resonance"]]
          <> [(91, clampI 0 127 (round (r * 127))) | Just r <- [lookupNum "room"]]
  case soundVoice snd' of
    Drum ps ->
      pure (MidiNote 9 (fromEnum ps + 35) vel (min secs 0.25) Nothing Percussion [])
    Melodic inst root -> do
      let degree = fromMaybe (num "n" 0) (lookupNum "note")
          speed = num "speed" 1
          shift = if speed > 0 && speed /= 1 then 12 * logBase 2 speed else 0
          key = clampI 0 127 (root + round (degree + shift))
          ch = trackChannel channel
      pure (MidiNote ch key vel (secs * 0.95) (Just (toGM inst)) inst controls)
  where
    vm = value e
    lookupNum k = Map.lookup k vm >>= valueDouble
    num k d = fromMaybe d (lookupNum k)
    cutoffCC c = clampI 0 127 (round (logBase (20000 / 50) (max 50 c / 50) * 127))

clampI :: Int -> Int -> Int -> Int
clampI lo hi = max lo . min hi

-- | @cycles@ cycles of the audible tracks as Euterpea music: one part per
-- track, each note placed by a rest from the start of the part. A cycle is a
-- whole note, and the tempo scales Euterpea's 120 quarter notes a minute to
-- the song's cycles per second.
songMusic :: Double -> Int -> [Compiled] -> Music1
songMusic cps cycles comps =
  tempo (toRational (2 * cps)) (foldr (:=:) (rest 0) (map part comps))
  where
    part c =
      let evs = sortOn wholeStart (filter eventHasOnset (eventsIn 0 (fromIntegral cycles) (cPattern c)))
          notes = mapMaybe (\e -> (,) e <$> eventNote cps (cChannel c) e) evs
          inst = case notes of
            (_, n) : _ -> mnInstrument n
            [] -> Percussion
          placed (e, n) =
            let at = wholeStart e
                len = min (wholeStop e - wholeStart e) (if mnChannel n == 9 then 1 / 8 else 4)
                body = note len (pitch (mnKey n), [Volume (mnVelocity n)])
             in if at > 0 then rest at :+: body else body
       in instrument inst (foldr ((:=:) . placed) (rest 0) notes)

-- | Write @cycles@ cycles of the audible tracks to a Standard MIDI File.
exportMidi :: FilePath -> Double -> Int -> [Compiled] -> IO ()
exportMidi path cps cycles comps = writeMidi path (songMusic cps cycles comps)
