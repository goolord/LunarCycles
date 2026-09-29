-- | A parameter knob that shows its modulation: the arc a signal sweeps, and
-- a dot where the signal is right now.
module Lunar.UI.Knob
  ( paramKnob
  , toNorm
  , fromNorm
  , formatParam
  ) where

import Control.Monad (when)
import Data.Text (Text)
import Lunar.Codegen (showNum)
import Lunar.Model
import NanoUI
import NanoUI.Path qualified as P

-- | A parameter value as a knob position in @[0, 1]@.
toNorm :: Param -> Double -> Double
toNorm p v
  | paramLog p = clamp01 (log (max lo v / lo) / log (hi / lo))
  | otherwise = clamp01 ((v - lo) / (hi - lo))
  where
    (lo, hi) = paramRange p

fromNorm :: Param -> Double -> Double
fromNorm p x
  | paramLog p = lo * (hi / lo) ** clamp01 x
  | otherwise = lo + clamp01 x * (hi - lo)
  where
    (lo, hi) = paramRange p

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

formatParam :: Param -> Double -> Text
formatParam p v
  | p == Cutoff && v >= 1000 = showNum (fromIntegral (round (v / 100) :: Int) / 10) <> "k"
  | p == Cutoff = showNum (fromIntegral (round v :: Int))
  | otherwise = showNum (fromIntegral (round (v * 100) :: Int) / 100)

-- | A knob for a parameter's base value. Drag up and down or turn the wheel
-- to move it; a right click puts it back to SuperDirt's default. @sweep@ is
-- the modulation's range as knob positions, and @live@ where the signal is
-- now. Returns the knob's response and the base value after this frame.
paramKnob ::
  Color -> Param -> Double -> Maybe (Double, Double) -> Maybe Double -> NanoUI (Response, Double)
paramKnob col p base sweep live = do
  let norm = toNorm p base
      d = 56 :: Float
      drawingKey =
        contentKey
          ( realToFrac norm
              : maybe [-1, -1] (\(a, b) -> [realToFrac a, realToFrac b]) sweep
              <> [maybe (-1) (realToFrac . toNorm p) live, if paramActive p (ParamSetting base SigNone 0 1) then 1 else 0]
          )
  resp <-
    canvasConfigured
      defaultCanvasConfig
        { canvasLayout = fixedWH d d defaultLayout
        , canvasContent = drawingKey
        , canvasCursor = Just (\_ _ _ -> UiCursorNsResize)
        , canvasFocusable = True
        }
      $ \(Rect x y w h) -> do
        cdc <- drawContext
        let th = cdcTheme cdc
            c = V2 (x + w / 2) (y + h / 2)
            r = min w h / 2 - 5
            start = 0.75 * pi
            travel = 1.5 * pi
            angleOf n = start + travel * realToFrac n
            onRing n rr = V2 (v2X c + rr * cos (angleOf n)) (v2Y c + rr * sin (angleOf n))
            face
              | cdcPressed cdc = styleActiveBg (themeButton th)
              | cdcHovered cdc = styleHoverBg (themeButton th)
              | otherwise = styleBg (themeButton th)
            active = abs (base - paramDefault p) > 1e-6
            line = (P.stroke 3) {P.strokeCap = P.RoundCap}
        drawStrokePathWith line (P.arc c r start travel) (P.Solid (withAlpha (styleBorder (themeButton th)) 0.4))
        case sweep of
          Just (a, b) ->
            drawStrokePathWith
              (P.stroke 5) {P.strokeCap = P.RoundCap}
              (P.arc c r (angleOf a) (travel * realToFrac (max 0.001 (b - a))))
              (P.Solid (withAlpha col 0.45))
          Nothing -> pure ()
        let from = toNorm p (paramDefault p)
            lo = min from norm
        drawStrokePathWith line (P.arc c r (angleOf lo) (travel * realToFrac (max 0.001 (abs (norm - from))))) (P.Solid (if active then col else themeMuted th))
        drawCircle c (r - 5) face
        when (cdcFocused cdc) $ drawStrokeCircle c (r + 3) 1.5 (themeFocusRing th)
        drawStrokeAA c (onRing norm (r - 7)) 2 (if active then col else themeMuted th)
        case live of
          Just v -> do
            drawCircle (onRing (toNorm p v) r) 3.5 (themeWindow th)
            drawCircle (onRing (toNorm p v) r) 2.5 (styleFg (themeButton th))
          Nothing -> pure ()
  drag <- useDrag2DOn resp
  (_, wheel) <- useWheelDeltaOn resp
  nav <- useKeyNav (respId resp)
  let keyboard = fromIntegral (fromEnum (knUp nav || knRight nav) - fromEnum (knDown nav || knLeft nav)) * 0.02
      delta = negate (realToFrac (v2Y (dragDelta drag))) / 160 + realToFrac wheel * 0.03 + keyboard
      next
        | respRightClicked resp = paramDefault p
        | delta /= 0 = fromNorm p (norm + delta)
        | otherwise = base
  pure (resp, next)
