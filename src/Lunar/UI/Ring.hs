-- | The cycle as a clock face: one ring per track, the cycle starting at
-- twelve o'clock and running clockwise, with the events of the cycle under
-- the playhead drawn as arcs. A track split by @pan@ (as @jux rev@ splits
-- one) draws its left channel on the inner half of its ring and its right
-- channel on the outer half.
module Lunar.UI.Ring
  ( ring
  ) where

import Control.Monad (forM_, when)
import Data.Text qualified as T
import Lunar.Codegen (showNum)
import Lunar.UI.Palette
import Lunar.UI.Timeline (Lane (..), evNum, smallFont)
import NanoUI
import NanoUI.Path qualified as P
import Sound.Tidal.Pattern (wholeStart, wholeStop)

ring :: Double -> Double -> Bool -> Maybe Int -> [Lane] -> NanoUI Response
ring cps now playing selected lanes = do
  let cycleText = T.pack (show (floor now :: Int))
      tempoText = showNum (fromIntegral (round (cps * 240) :: Int)) <> " bpm"
      valueFont = defaultTextFont {textFontSize = 32, textFontWeight = WeightMedium}
  labelMetrics <- resolveFontUi 12 WeightNormal FontStyleNormal FontRegular
  valueMetrics <- resolveFontUi 32 WeightMedium FontStyleNormal FontRegular
  labelWidth <- max <$> lineWidthUi labelMetrics "Cycle" <*> lineWidthUi labelMetrics tempoText
  valueWidth <- lineWidthUi valueMetrics cycleText
  canvas (fillW . fillH) $ \(Rect x y w h) -> do
    cdc <- drawContext
    let th = cdcTheme cdc
        c = V2 (x + w / 2) (y + h / 2)
        size = min w h
        outer = max 1 (size / 2 - 18)
        inner = max (outer * 0.48) (min 52 (outer * 0.52))
        well = max 0 (inner - 4)
        n = max 1 (length lanes)
        band = (outer - inner) / fromIntegral n
        cyc = fromIntegral (floor now :: Int) :: Double
        angleOf t = realToFrac (t - cyc) * 2 * pi - pi / 2
        fg = styleFg (themePanel th)
    drawPath (P.circle c (outer + 14)) (themeWindow th)
    -- Sixteen step ticks, the four beats longer.
    forM_ [0 .. 15 :: Int] $ \k -> do
      let a = fromIntegral k / 16 * 2 * pi - pi / 2
          long = k `mod` 4 == 0
          r0 = outer + 4
          r1 = outer + (if long then 12 else 8)
      if long && size >= 230
        then drawTextWith smallFont (V2 (v2X c + (outer + 10) * cos a) (v2Y c + (outer + 10) * sin a)) AlignCenter AlignMiddle (T.pack (show (k `div` 4 + 1))) (themeMuted th)
        else drawStrokeAA (V2 (v2X c + r0 * cos a) (v2Y c + r0 * sin a)) (V2 (v2X c + r1 * cos a) (v2Y c + r1 * sin a)) (if long then 2 else 1) (themeMuted th)
    forM_ (zip [0 :: Int ..] lanes) $ \(i, lane) -> do
      let rMid = outer - band * (fromIntegral i + 0.5)
          col = laneColor lane
          dim = if laneSilenced lane then 0.25 else 1
          panned = any (\e -> abs (evNum "pan" 0.5 e - 0.5) > 0.05) (laneEvents lane)
          thick = band * (if panned then 0.32 else 0.62)
      drawStrokePath (P.arc c rMid 0 (2 * pi)) (band * 0.78) (withAlpha col 0.10)
      when (selected == Just i) $
        drawStrokePath (P.arc c (rMid + band * 0.44) 0 (2 * pi)) 1 (withAlpha col 0.65)
      forM_ (laneEvents lane) $ \e -> do
        let ws = max cyc (fromRational (wholeStart e))
            we = min (cyc + 1) (fromRational (wholeStop e))
            live = fromRational (wholeStart e) <= now && now < fromRational (wholeStop e)
            r = rMid + (if panned then realToFrac (evNum "pan" 0.5 e - 0.5) * band * 0.9 else 0)
            gain = max 0.35 (min 1 (evNum "gain" 1 e / 1.2))
            sweep = max 0.02 (angleOf we - angleOf ws - 0.025)
        when (we > ws) $ do
          drawStrokePathWith
            (P.stroke thick) {P.strokeCap = P.ButtCap}
            (P.arc c r (angleOf ws + 0.0125) sweep)
             (P.Solid (withAlpha col (dim * gain * (if live && playing then 1 else 0.82))))
          -- The onset, so a run of long events still reads as separate hits.
          when (fromRational (wholeStart e) >= cyc) $ do
            let a0 = angleOf ws + 0.0125
                at rr = V2 (v2X c + rr * cos a0) (v2Y c + rr * sin a0)
            drawStrokeAA (at (r - thick / 2)) (at (r + thick / 2)) 2.5 (withAlpha col dim)
        when (live && playing && we > ws) $
          drawStrokePath (P.arc c (r + thick / 2 + 1.5) (angleOf ws + 0.0125) sweep) 1.5 fg
    -- Playhead.
    let a = angleOf now
        tip = V2 (v2X c + (outer + 6) * cos a) (v2Y c + (outer + 6) * sin a)
        base = V2 (v2X c + inner * cos a) (v2Y c + inner * sin a)
    drawStrokeAA base tip 1.5 (themeAccent th)
    drawCircle tip 3.5 (themeAccent th)
    drawPath (P.circle c well) (themeWindow th)
    moonPhase c (well * 0.86) (now - cyc) (themeAccent th)
    -- Fit the readout in a square inscribed in the clear centre, not in the
    -- canvas bounds. On small panes, keep only the cycle count legible.
    let half = max 0 (well / sqrt 2 - 3)
        labelH = fmLineHeight labelMetrics
        valueH = fmLineHeight valueMetrics
        readoutH = 2 * labelH + valueH + 8
        fullScale = min 1 (min (2 * half / max 1 (max labelWidth valueWidth)) (2 * half / readoutH))
        countScale = min 1 (min (2 * half / max 1 valueWidth) (2 * half / valueH))
        textAt font dy txt col = drawTextWith font (V2 (v2X c) (v2Y c + dy)) AlignCenter AlignMiddle txt col
    withClip (Rect (v2X c - half) (v2Y c - half) (2 * half) (2 * half)) $
      if fullScale >= 0.85
        then do
          let offset = (valueH / 2 + 4 + labelH / 2) * fullScale
          textAt smallFont {textFontSize = 12 * fullScale} (-offset) "Cycle" (themeMuted th)
          textAt valueFont {textFontSize = 32 * fullScale} 0 cycleText fg
          textAt smallFont {textFontSize = 12 * fullScale} offset tempoText (themeMuted th)
        else when (32 * countScale >= 12) $
          textAt valueFont {textFontSize = 32 * countScale} 0 cycleText fg

-- | The moon behind the readout, lit as far through its phases as the
-- playhead is through the cycle: new at twelve o'clock, full at six. It
-- waxes from the right and wanes to the left, and stays faint enough for
-- the cycle count to read over it.
moonPhase :: V2 -> Float -> Double -> Color -> CanvasM ()
moonPhase c rad phase col = when (rad > 8) $ do
  let p = realToFrac (phase - fromIntegral (floor phase :: Int)) :: Float
      waxing = p < 0.5
      k = cos (2 * pi * p)
      side = if waxing then 1 else -1
      samples = 40 :: Int
      ys = [rad * (2 * fromIntegral j / fromIntegral samples - 1) | j <- [0 .. samples]]
      halfW y = sqrt (max 0 (rad * rad - y * y))
      at x y = V2 (v2X c + side * x) (v2Y c + y)
      limb = [at (halfW y) y | y <- ys]
      terminator = [at (k * halfW y) y | y <- reverse ys]
      lighted = 1 - k
      silver = lerpColor col (colorRGBA 255 255 255 255) 0.6
  drawPath (P.circle c rad) (withAlpha silver 0.04)
  drawStrokePath (P.arc c rad 0 (2 * pi)) 1 (withAlpha silver 0.14)
  when (lighted > 0.01) $ do
    drawPathWith P.NonZero (P.polygon (limb <> terminator)) (P.Solid (withAlpha silver 0.13))
    drawStrokePath (P.arc c rad (if waxing then -pi / 2 else pi / 2) pi) 1.5 (withAlpha col 0.7)
