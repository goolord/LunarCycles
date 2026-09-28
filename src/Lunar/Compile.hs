-- | A song's tracks as tidal-core patterns, built from the same pieces as
-- the code "Lunar.Codegen" writes: the mini-notation is parsed by Tidal's
-- own parser, and each transform is the Tidal function of the same name.
module Lunar.Compile
  ( Compiled (..)
  , compileSong
  , compileTrack
  , audible
  , transformFn
  , signalPattern
  , eventsIn
  , sampleAt
  , valueDouble
  , valueText
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Lunar.Catalog (isPitched)
import Lunar.Codegen (ParamExpr (..), paramExpr, sourceMini)
import Lunar.Model
import Sound.Tidal.Control (chop, hurry, striate)
import Sound.Tidal.Core (every, (#))
import Sound.Tidal.Core qualified as Core
import Sound.Tidal.Params qualified as P
import Sound.Tidal.ParseBP (parseBP)
import Sound.Tidal.Pattern
import Sound.Tidal.UI qualified as UI

data Compiled = Compiled
  { cTrack :: !Track
  , cChannel :: !Int
  , cPattern :: ControlPattern
    -- ^ The whole track: source, transforms and parameters.
  , cError :: !(Maybe Text)
    -- ^ Why the mini-notation did not parse; the pattern is silent then.
  , cSignals :: ![(Param, Pattern Double)]
    -- ^ Each modulated parameter's signal, for reading its value live.
  }

compileSong :: Song -> [Compiled]
compileSong song = zipWith compileTrack [1 ..] (songTracks song)

compileTrack :: Int -> Track -> Compiled
compileTrack channel t =
  Compiled
    { cTrack = t
    , cChannel = channel
    , cPattern = foldr (transformFn . snd) (withParams base) (trackChain t)
    , cError = err
    , cSignals = [(p, signalOf e) | (p, s) <- params, let e = paramExpr p s, isRange e]
    }
  where
    pitched = isPitched (trackSound t)
    mini = T.unpack (sourceMini t)
    (base, err)
      | pitched = case parseBP mini of
          Left e -> (silence, Just (T.pack (show e)))
          Right notes -> (P.note notes # P.s (pure (T.unpack (trackSound t))), Nothing)
      | otherwise = case parseBP mini of
          Left e -> (silence, Just (T.pack (show e)))
          Right names -> (P.s names, Nothing)
    params = [(p, s) | (p, s) <- Map.toList (trackParams t), paramActive p s]
    withParams pat = foldl (\acc (p, s) -> acc # paramFn p (signalOf (paramExpr p s))) pat params
    isRange PRange {} = True
    isRange _ = False
    signalOf = \case
      PConst v -> pure v
      PRange lo hi sig period -> UI.range (pure lo) (pure hi) (signalPattern sig period)

-- | The tracks that sound: none muted, and only the soloed ones when any is.
audible :: [Compiled] -> [Compiled]
audible cs =
  let anySolo = any (trackSolo . cTrack) cs
   in filter (\c -> not (trackMuted (cTrack c)) && (not anySolo || trackSolo (cTrack c))) cs

signalPattern :: Signal -> Rational -> Pattern Double
signalPattern sig period = case sig of
  SigRand -> UI.rand
  SigNone -> pure 0.5
  _ -> timed $ case sig of
    SigSine -> Core.sine
    SigTri -> Core.tri
    SigSaw -> Core.saw
    SigSquare -> Core.square
    _ -> UI.perlin
  where
    timed p
      | period == 1 = p
      | otherwise = slow (pure period) p

paramFn :: Param -> Pattern Double -> ControlPattern
paramFn = \case
  Cutoff -> P.cutoff
  Resonance -> P.resonance
  Gain -> P.gain
  Pan -> P.pan
  Speed -> P.speed
  Begin -> P.begin
  End -> P.end
  Room -> P.room
  Shape -> P.shape

transformFn :: Transform -> ControlPattern -> ControlPattern
transformFn = \case
  Fast r -> fast (pure r)
  Slow r -> slow (pure r)
  Rev -> rev
  Palindrome -> UI.palindrome
  Brak -> UI.brak
  Iter n -> UI.iter (pure n)
  Ply n -> UI.ply (pure (fromIntegral n))
  Rot n -> UI.rot (pure n)
  Degrade p -> UI.degradeBy (pure p)
  Chop n -> chop (pure n)
  Striate n -> striate (pure n)
  Hurry r -> hurry (pure r)
  Jux t -> UI.jux (transformFn t)
  Every n t -> every (pure n) (transformFn t)
  Whenmod a b t -> UI.whenmod (pure (fromIntegral a)) (pure (fromIntegral b)) (transformFn t)
  Sometimes t -> UI.sometimesBy 0.5 (transformFn t)
  Off r t -> UI.off (pure r) (transformFn t)

-- | The events with a whole that overlap @[from, to)@, in time order.
eventsIn :: Double -> Double -> ControlPattern -> [Event ValueMap]
eventsIn from to pat = filter isDigital (queryArc pat (Arc (toRational from) (toRational to)))

-- | A continuous pattern's value at a moment.
sampleAt :: Pattern Double -> Double -> Maybe Double
sampleAt pat c = case queryArc pat (Arc (toRational c) (toRational c)) of
  e : _ -> Just (value e)
  [] -> Nothing

valueDouble :: Value -> Maybe Double
valueDouble = \case
  VF v -> Just v
  VN n -> Just (unNote n)
  VI i -> Just (fromIntegral i)
  VR r -> Just (fromRational r)
  _ -> Nothing

valueText :: Value -> Text
valueText = \case
  VS s -> T.pack s
  v -> maybe "…" (T.pack . show) (valueDouble v)
