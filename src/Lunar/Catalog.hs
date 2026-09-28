-- | The sounds a track can play: a SuperDirt sample or synth name, and the
-- General MIDI sound Euterpea plays for it.
module Lunar.Catalog
  ( Voice (..)
  , Sound (..)
  , catalog
  , catalogNames
  , lookupSound
  , isPitched
  ) where

import Data.List (find)
import Data.Text (Text)
import Euterpea.Music (InstrumentName (..), PercussionSound (..))

-- | How a sound reaches MIDI: a key on the percussion channel, or a melodic
-- instrument whose pitch follows the pattern's note.
data Voice
  = Drum !PercussionSound
  | Melodic !InstrumentName !Int
    -- ^ Instrument and the MIDI key that note 0 plays.
  deriving (Eq, Show)

data Sound = Sound
  { soundName :: !Text
  , soundLabel :: !Text
  , soundVoice :: !Voice
  }

-- | The sounds offered by a track's sound menu. Names are from the
-- Dirt-Samples bank and SuperDirt's bundled synths, so the code runs against
-- a stock SuperDirt.
catalog :: [Sound]
catalog =
  [ Sound "bd" "bd · kick" (Drum BassDrum1)
  , Sound "sn" "sn · snare" (Drum AcousticSnare)
  , Sound "cp" "cp · clap" (Drum HandClap)
  , Sound "hh" "hh · closed hat" (Drum ClosedHiHat)
  , Sound "oh" "oh · open hat" (Drum OpenHiHat)
  , Sound "lt" "lt · low tom" (Drum LowTom)
  , Sound "mt" "mt · mid tom" (Drum LowMidTom)
  , Sound "ht" "ht · high tom" (Drum HighTom)
  , Sound "cr" "cr · crash" (Drum CrashCymbal1)
  , Sound "rm" "rm · rimshot" (Drum SideStick)
  , Sound "cb" "cb · cowbell" (Drum Cowbell)
  , Sound "tabla" "tabla" (Drum HiBongo)
  , Sound "superpiano" "superpiano" (Melodic AcousticGrandPiano 60)
  , Sound "supersaw" "supersaw" (Melodic Lead2Sawtooth 48)
  , Sound "superfm" "superfm" (Melodic RhodesPiano 60)
  , Sound "superhammond" "superhammond" (Melodic HammondOrgan 60)
  , Sound "bass3" "bass3 · synth bass" (Melodic SynthBass1 36)
  , Sound "arpy" "arpy" (Melodic Harpsichord 60)
  , Sound "pluck" "pluck" (Melodic PizzicatoStrings 60)
  , Sound "superchip" "superchip" (Melodic Lead1Square 60)
  ]

catalogNames :: [Text]
catalogNames = map soundName catalog

lookupSound :: Text -> Maybe Sound
lookupSound name = find ((== name) . soundName) catalog

-- | Pitched sounds are patterned with @note@ rather than sample numbers.
isPitched :: Text -> Bool
isPitched name = case soundVoice <$> lookupSound name of
  Just (Melodic _ _) -> True
  _ -> False
