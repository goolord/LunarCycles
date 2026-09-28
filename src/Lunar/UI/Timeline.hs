-- | The multi-track timeline: a page of cycles with one lane per track,
-- drawn from the events tidal-core returns for that page. Events move up
-- and down their lane with @pan@ (so @jux rev@ splits a lane in two) and
-- fade with @gain@; pitched tracks draw as a piano roll. A modulated
-- parameter's signal is traced across its lane.
module Lunar.UI.Timeline
  ( Lane (..)
  , timeline
  , laneAt
  , evNum
  , eventLabel
  , smallFont
  , monoFont
  ) where

import Control.Monad (forM_, when)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lunar.Compile (sampleAt, valueDouble, valueText)
import Lunar.Model (Param (..), paramName)
import Lunar.UI.Knob (toNorm)
import Lunar.UI.Palette
import NanoUI
import NanoUI.Path qualified as P
import Sound.Tidal.Pattern (Event, EventF (..), Pattern, ValueMap, wholeStart, wholeStop)

-- | One track's events on the page, and how to draw them.
data Lane = Lane
  { laneName :: !Text
  , laneColor :: !Color
  , laneEvents :: ![Event ValueMap]
  , lanePitched :: !Bool
  , laneSignal :: !(Maybe (Param, Pattern Double))
  , laneSilenced :: !Bool
  }

smallFont :: TextFont
smallFont = defaultTextFont {textFontSize = 12}

monoFont :: TextFont
monoFont = defaultTextFont {textFontSize = 11, textFontVariant = FontMono}

evNum :: Text -> Double -> Event ValueMap -> Double
evNum k d e = fromMaybe d (Map.lookup (T.unpack k) (value e) >>= valueDouble)

-- | An event's values as @s bd · n 2 · gain 1.1@.
eventLabel :: Event ValueMap -> Text
eventLabel e = T.intercalate "  ·  " [T.pack k <> " " <> valueText v | (k, v) <- Map.toList (value e)]

-- | The space above the lanes, for the cycle numbers.
headerH :: Float
headerH = 28

-- | Which of @n@ lanes a point in the timeline's rect falls in.
laneAt :: Rect -> Int -> V2 -> Maybe Int
laneAt (Rect _ y _ h) n (V2 _ py)
  | n <= 0 || py < y + headerH || py >= y + h - 4 = Nothing
  | otherwise = Just (min (n - 1) (floor ((py - y - headerH) / ((h - headerH - 4) / fromIntegral n))))

-- | A page of @pageSpan@ cycles from @start@, with the playhead at @now@ and
-- the lane at @selected@ outlined.
timeline :: Double -> Double -> Double -> Bool -> Maybe Int -> [Lane] -> NanoUI Response
timeline start pageSpan now running selected lanes = do
  mouse <- uiMousePos
  canvasConfigured
    defaultCanvasConfig
      { canvasLayout = fillW (fillH defaultLayout)
      , canvasTrackPointer = True
      }
    $ \(Rect x y w h) -> do
      cdc <- drawContext
      let th = cdcTheme cdc
          gutter = if w < 500 then 66 else 92
          header = headerH
          gx = x + gutter
          gw = max 1 (w - gutter - 8)
          n = max 1 (length lanes)
          laneH = (h - header - 4) / fromIntegral n
          xOf c = gx + realToFrac ((c - start) / pageSpan) * gw
          fg = styleFg (themePanel th)
          hovered = ref mouse
          ref (V2 mx my) =
            find
              (\(_, Rect ex ey ew eh) -> mx >= ex && mx <= ex + ew && my >= ey - 2 && my <= ey + eh + 2)
              (reverse placed)
          placed = concat (zipWith lanePlacement [0 :: Int ..] lanes)
          lanePlacement i lane =
            let ly = y + header + fromIntegral i * laneH
             in [(e, eventRect lane ly e) | e <- laneEvents lane]
          eventRect lane ly e =
            let ws = fromRational (wholeStart e)
                we = fromRational (wholeStop e)
                ex0 = xOf ws
                ex1 = max (ex0 + 3) (xOf we - 1.5)
             in if lanePitched lane
                  then
                    let notes = mapMaybe (\ev -> Map.lookup "note" (value ev) >>= valueDouble) (laneEvents lane)
                        lo = if null notes then 0 else minimum notes
                        hi = max (lo + 12) (if null notes then 12 else maximum notes)
                        nh = max 4 (min 9 (laneH / 8))
                        frac = realToFrac ((evNum "note" 0 e - lo) / (hi - lo))
                        cy = ly + laneH - 6 - frac * (laneH - 12)
                     in Rect ex0 (cy - nh / 2) (ex1 - ex0) nh
                  else
                    let panned = any (\ev -> abs (evNum "pan" 0.5 ev - 0.5) > 0.05) (laneEvents lane)
                        eh = min 32 (laneH * (if panned then 0.25 else 0.44))
                        cy = ly + laneH / 2 + realToFrac (evNum "pan" 0.5 e - 0.5) * laneH * 0.5
                     in Rect ex0 (cy - eh / 2) (ex1 - ex0) eh
      drawRoundedRect (Rect x y w h) 4 (themeWindow th)
      -- Cycle and beat rules, and the cycle numbers above them.
      let firstBeat = ceiling (start * 4) :: Int
          lastBeat = floor ((start + pageSpan) * 4) :: Int
          beatStep = if pageSpan > 4 then 4 else 1
      forM_ [firstBeat .. lastBeat] $ \b -> do
        let c = fromIntegral b / 4
            bx = xOf c
            isCycle = b `mod` 4 == 0
        when (isCycle || b `mod` beatStep == 0) $
          drawRect (Rect bx (y + header) (if isCycle then 1.5 else 1) (h - header - 4)) (withAlpha (themeSeparator th) (if isCycle then 1 else 0.45))
        when (isCycle && bx + 48 < x + w) $
          drawTextWith smallFont (V2 (bx + 6) (y + 5)) AlignStart AlignTop (T.pack (show (b `div` 4))) (themeMuted th)
      drawTextWith smallFont (V2 (x + 10) (y + 5)) AlignStart AlignTop "Cycle" (themeMuted th)
      forM_ (zip [0 :: Int ..] lanes) $ \(i, lane) -> do
        let ly = y + header + fromIntegral i * laneH
            col = laneColor lane
            dimmed = laneSilenced lane
            alphaFor e = (if dimmed then 0.25 else 1) * max 0.35 (min 1 (evNum "gain" 1 e / 1.2))
        drawRect (Rect (x + 4) (ly + laneH - 1) (w - 8) 1) (withAlpha (themeSeparator th) 0.5)
        when (selected == Just i) $ do
          drawRect (Rect (x + 4) ly (w - 8) laneH) (withAlpha col 0.08)
        withClip (Rect (x + 8) ly (gutter - 14) laneH) $
          drawTextWith smallFont {textFontWeight = if selected == Just i then WeightMedium else WeightNormal} (V2 (x + 10) (ly + laneH / 2)) AlignStart AlignMiddle (laneName lane) (if dimmed then themeMuted th else col)
        withClip (Rect gx (y + header) gw (h - header)) $ do
          forM_ (laneEvents lane) $ \e -> do
            let r = eventRect lane ly e
                ws = fromRational (wholeStart e)
                we = fromRational (wholeStop e)
                playing = running && ws <= now && now < we
            drawRoundedRect r 2 (withAlpha col (alphaFor e * (if playing then 1 else 0.75)))
            drawRect (Rect (rectX r) (rectY r) (min 2 (rectW r)) (rectH r)) (withAlpha col (if dimmed then 0.3 else 1))
            when playing $ drawStrokeRoundedRect (rectInflate 1 r) 2 1 fg
          case laneSignal lane of
            Just (p, sig) -> do
              let steps = 160 :: Int
                  pts =
                    [ V2 (xOf c) (ly + laneH - 4 - realToFrac (toNorm p v) * (laneH - 8))
                    | k <- [0 .. steps]
                    , let c = start + pageSpan * fromIntegral k / fromIntegral steps
                    , Just v <- [sampleAt sig c]
                    ]
              drawStrokePath (P.polyline pts) 1.5 (withAlpha fg (if dimmed then 0.2 else 0.55))
              drawTextWith smallFont (V2 (gx + gw - 4) (ly + 3)) AlignEnd AlignTop ("~ " <> paramName p) (themeMuted th)
            Nothing -> pure ()
      -- Playhead.
      when (now >= start && now <= start + pageSpan) $ do
        let px = xOf now
        drawRect (Rect (px - 1) (y + header - 4) 2 (h - header)) (themeAccent th)
        drawCircle (V2 px (y + header - 4)) 4 (themeAccent th)
      case hovered of
        Just (e, r) -> do
          drawStrokeRoundedRect (rectInflate 2 r) 4 1.5 fg
          let txt = eventLabel e
              tw = min (w - 8) (lineWidth (cdcFont cdc) txt + 16)
              V2 mx my = mouse
              bx = max (x + 4) (min (x + w - tw - 4) (mx + 12))
              by = if my + 40 > y + h then my - 36 else my + 14
          drawRoundedRect (Rect bx by tw 26) 6 (styleBg (themeFloatingWindow th))
          drawStrokeRoundedRect (Rect bx by tw 26) 6 1 (styleBorder (themeFloatingWindow th))
          withClip (Rect (bx + 4) by (tw - 8) 26) $
            drawText (V2 (bx + 8) (by + 13)) AlignStart AlignMiddle txt (styleFg (themeFloatingWindow th))
        Nothing -> pure ()
