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

import Data.List (nub, sortOn)
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
    -- ^ Controller values sent before the note, in order: the filter, then on
    -- melodic channels pan (10) and reverb send (91).
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
      filt = filterControls (num "cutoff" 20000) (num "resonance" 0)
      controls =
        filt
          <> [(10, clampI 0 127 (round (num "pan" 0.5 * 127)))]
          <> [(91, clampI 0 127 (round (r * 127))) | Just r <- [lookupNum "room"]]
  case soundVoice snd' of
    Drum ps ->
      pure (MidiNote 9 (fromEnum ps + 35) vel (min secs 0.25) Nothing Percussion filt)
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

-- | The low-pass filter at a cutoff in Hz and a resonance in SuperDirt's 0 to
-- 0.9. Every note carries it, even at the defaults, so a channel does not keep
-- the last track's filter. Brightness (74) and resonance (71) are for
-- hardware synths; FluidSynth ignores them, so the same values also go as
-- SoundFont NRPNs, which offset the preset's filter generators: cutoff (8) in
-- cents from the usual unfiltered 13500, resonance (9) in centibels.
filterControls :: Double -> Double -> [(Int, Int)]
filterControls cutoff res =
  [ (74, clampI 0 127 (round (logBase (20000 / 50) (max 50 cutoff / 50) * 127)))
  , (71, clampI 0 127 (round (res / 0.9 * 127)))
  ]
    <> sfNrpn 8 (round ((cents - 13500) / 2))
    <> sfNrpn 9 (round (res / 0.9 * 240))
  where
    cents = 1200 * logBase 2 (max 50 cutoff / 8.176)

-- | A SoundFont 2 NRPN: select generator @gen@ and offset it by @steps@ of the
-- generator's own unit (2 cents for cutoff, 1 centibel for resonance).
sfNrpn :: Int -> Int -> [(Int, Int)]
sfNrpn gen steps =
  let v = clampI 0 16383 (8192 + steps)
   in [(99, 120), (98, gen), (6, v `div` 128), (38, v `mod` 128)]

clampI :: Int -> Int -> Int -> Int
clampI lo hi = max lo . min hi

-- | @cycles@ cycles of the audible tracks as Euterpea music: one part per
-- instrument a track plays, as live playback routes each note by its own
-- sound, and each note placed by a rest from the start of the part. A cycle
-- is a whole note, and the tempo scales Euterpea's 120 quarter notes a
-- minute to the song's cycles per second.
songMusic :: Double -> Int -> [Compiled] -> Music1
songMusic cps cycles comps =
  tempo (toRational (2 * cps)) (foldr (:=:) (rest 0) (concatMap parts comps))
  where
    parts c =
      let evs = sortOn wholeStart (filter eventHasOnset (eventsIn 0 (fromIntegral cycles) (cPattern c)))
          notes = mapMaybe (\e -> (,) e <$> eventNote cps (cChannel c) e) evs
          placed (e, n) =
            let at = wholeStart e
                len = min (wholeStop e - wholeStart e) (if mnChannel n == 9 then 1 / 8 else 4)
                body = note len (pitch (mnKey n), [Volume (mnVelocity n)])
             in if at > 0 then rest at :+: body else body
       in [ instrument inst (foldr ((:=:) . placed) (rest 0) (filter ((== inst) . mnInstrument . snd) notes))
          | inst <- nub (map (mnInstrument . snd) notes)
          ]

-- | Write @cycles@ cycles of the audible tracks to a Standard MIDI File.
exportMidi :: FilePath -> Double -> Int -> [Compiled] -> IO ()
exportMidi path cps cycles comps = writeMidi path (songMusic cps cycles comps)
