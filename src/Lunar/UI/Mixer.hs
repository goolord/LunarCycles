module Lunar.UI.Mixer (mixerView) where

import Control.Monad (forM_, when)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Lunar.Compile (Compiled (..), sampleAt)
import Lunar.Model
import Lunar.UI.Knob
import Lunar.UI.Palette (trackColor)
import Lunar.UI.Timeline (smallFont)
import NanoUI

-- Paging keeps every fader usable even when the pane is narrow. The detail
-- area follows channel selection, not the sequence's set of active parts.
mixerView :: Float -> Double -> [Compiled] -> Maybe Int -> (Int -> NanoUI ()) -> (Int -> (Track -> Track) -> NanoUI ()) -> (Int -> Compiled -> NanoUI ()) -> NanoUI ()
mixerView width now compiled selected selectChannel editChannel details = do
  th <- uiTheme
  (bank, setBank) <- useInt 0
  let beside = width >= 1000
      bankWidth = if beside then width - 460 else width
      capacity = max 1 (floor ((bankWidth + 8) / 120))
      count = length compiled
      lastBank = max 0 ((count - 1) `div` capacity)
      page = min bank lastBank
      start = page * capacity
      visible = take capacity (drop start (zip [0 ..] compiled))
      chosen = find ((== selected) . Just . trackId . cTrack . snd) (zip [0 ..] compiled)
      channelBank = columnWith (tight . fillW . gap 18) $ do
        labelWith (fontMuted . fontSize 12 . fillW) "Channels shared across all sequences"
        when (count > capacity) $
          rowWith (tight . fillW . gap 8 . alignMid) $ do
            whenM (disabledWhen (page == 0) (buttonWith (minH 44 . alignMid) "Previous")) (setBank (page - 1))
            labelWith (fontMono . fontSize 12 . alignMid) (T.pack (show (start + 1) <> "-" <> show (min count (start + capacity)) <> " / " <> show count))
            whenM (disabledWhen (page == lastBank) (buttonWith (minH 44 . alignMid) "Next")) (setBank (page + 1))
        rowWith (tight . fillW . gap 8) $
          forM_ visible $ \(i, c) -> withKey (trackId (cTrack c)) $ do
            let t = cTrack c
                tid = trackId t
                col = trackColor th i
                picked = selected == Just tid
                setting p = Map.findWithDefault (defaultSetting p) p (trackParams t)
                store p v = editChannel tid $ \tr ->
                  let s = (Map.findWithDefault (defaultSetting p) p (trackParams tr)) {psBase = v}
                   in tr {trackParams = if paramActive p s then Map.insert p s (trackParams tr) else Map.delete p (trackParams tr)}
                live p = lookup p (cSignals c) >>= \sig -> sampleAt sig now
                surface = panelStyle (background (themeWindow th) . borderWidth 1 . borderColor (if picked then col else themeSeparator th) . cornerRadius 3)
            styled surface $ panelWith (fixedW 112 . padAll 8 . gap 6) $ do
              labelWith (fontMono . fontSize 12 . fontColor col . fillW . alignCenter) ("d" <> T.pack (show (i + 1)))
              fm <- uiFontMetrics
              name <- truncateTextUi fm 80 (trackName t)
              r <- styled (if picked then tinted (const col) else id) $ buttonWith' (fillW . minH 44 . fontSize 13 . alignMid) name
              tooltip r ("Select " <> trackName t <> " controls")
              when (respClicked r) (selectChannel tid)
              sound <- truncateTextUi fm 90 (trackSound t)
              soundResp <- labelWith' (fontMuted . fontSize 12 . fillW . alignCenter) sound
              tooltip soundResp (trackSound t)
              labelWith (fontMuted . fontSize 12 . fillW . alignCenter) "Gain"
              let gain = setting Gain
              (gr, g) <- gainFader (if width < 600 then 156 else 204) col (psBase gain) (if psSignal gain == SigNone then Nothing else live Gain)
              tooltip gr "Gain multiplier: drag, scroll or use arrow keys. Right-click to reset to 1."
              when (respPressed gr && not picked) (selectChannel tid)
              when (g /= psBase gain) (store Gain g)
              labelWith (fontMono . fontSize 13 . fillW . alignCenter) (formatParam Gain (psBase gain) <> "x")
              rowWith (tight . gap 6 . fillW) $ do
                (mr, m) <- toggleButtonWith' (fixedWH 44 44 . fontSize 12 . alignMid) Warning "M" (trackMuted t)
                tooltip mr (if trackMuted t then "Unmute " <> trackName t else "Mute " <> trackName t)
                when (m /= trackMuted t) (editChannel tid (\tr -> tr {trackMuted = m}))
                (sr, s) <- toggleButtonWith' (fixedWH 44 44 . fontSize 12 . alignMid) Accent "S" (trackSolo t)
                tooltip sr (if trackSolo t then "Stop soloing " <> trackName t else "Solo " <> trackName t)
                when (s /= trackSolo t) (editChannel tid (\tr -> tr {trackSolo = s}))
              labelWith (fontMuted . fontSize 12 . fillW . alignCenter) "Pan"
              columnWith (tight . fillW . alignCenter . gap 4) $ do
                let pan = setting Pan
                (pr, p) <- paramKnob col Pan (psBase pan) Nothing (live Pan)
                tooltip pr "Pan: drag, scroll or use arrow keys. Right-click to centre."
                when (respPressed pr && not picked) (selectChannel tid)
                when (p /= psBase pan) (store Pan p)
                labelWith (fontMono . fontSize 12 . alignCenter) (panText (psBase pan))
              let status
                    | trackMuted t = "Muted"
                    | any (trackSolo . cTrack) compiled && not (trackSolo t) = "Excluded"
                    | trackSolo t = "Solo"
                    | psSignal gain /= SigNone = "Gain modulated"
                    | otherwise = ""
              labelWith (fontMuted . fontSize 11 . fillW . alignCenter . fixedH 18) status
      channelDetails = columnWith (tight . fillW . gap 18) $
        forM_ chosen $ \(i, c) -> withKey (trackId (cTrack c)) $ do
          labelWith (fontMedium . fontSize 15 . fillW) ("Channel controls: " <> trackName (cTrack c))
          details i c
  styled (inputStyle (background (styleBg (themePanel th)) . borderWidth 0)) $
    scrollWith (fillW . fillH) $ columnWith (tight . fillW . gap 18 . padRight 4) $
      if null compiled
        then labelWith (fontMuted . fontSize 14 . fillW) "No channels to mix. Add a track in Pattern or Tracks to give it a fader."
        else if beside
          then rowWith (tight . fillW . gap 28) $ do
            channelBank
            columnWith (tight . fixedW 432) channelDetails
          else do
            channelBank
            separator
            channelDetails
  where
    panText v
      | abs (v - 0.5) < 0.005 = "Centre"
      | otherwise = T.pack (show (round (abs (v - 0.5) * 200) :: Int)) <> if v < 0.5 then "% L" else "% R"

-- The scale is Tidal's linear gain multiplier, not a measured audio level.
-- A separate marker shows modulation without moving the editable base.
gainFader :: Float -> Color -> Double -> Maybe Double -> NanoUI (Response, Double)
gainFader height col base live = do
  resp <- canvasConfigured defaultCanvasConfig
    { canvasLayout = fillW (fixedH height defaultLayout)
    , canvasContent = contentKey [realToFrac base, maybe (-1) realToFrac live]
    , canvasCursor = Just (\_ _ _ -> UiCursorNsResize)
    , canvasFocusable = True
    } $ \r@(Rect x y w h) -> do
      cdc <- drawContext
      let th = cdcTheme cdc
          cx = x + w * 0.62
          yOf v = y + 12 + (h - 24) * realToFrac (1 - toNorm Gain v)
          rail = Rect (cx - 3) (y + 12) 6 (h - 24)
          thumb = Rect (cx - 17) (yOf base - 7) 34 14
      drawRoundedRect rail 3 (styleBg (themeButton th))
      forM_ [0, 0.5, 1, 1.5] $ \v -> do
        drawTextWith smallFont (V2 (x + 2) (yOf v)) AlignStart AlignMiddle (formatParam Gain v) (themeMuted th)
        drawStrokeAA (V2 (cx - 11) (yOf v)) (V2 (cx + 11) (yOf v)) 1 (styleBorder (themeButton th))
      forM_ live $ \v -> drawCircle (V2 (cx + 24) (yOf v)) 3 col
      drawRoundedRect thumb 3 (if cdcHovered cdc || cdcPressed cdc then styleHoverBg (themeButton th) else styleBg (themeButton th))
      drawStrokeRoundedRect thumb 3 1 col
      drawStrokeAA (V2 (cx - 12) (yOf base)) (V2 (cx + 12) (yOf base)) 2 (styleFg (themeButton th))
      when (cdcFocused cdc) (drawStrokeRoundedRect r 3 2 (themeFocusRing th))
  drag <- useDrag2DOn resp
  (_, wheel) <- useWheelDeltaOn resp
  nav <- useKeyNav (respId resp)
  let keyboard = fromIntegral (fromEnum (knUp nav || knRight nav) - fromEnum (knDown nav || knLeft nav)) * 0.01
      delta = negate (realToFrac (v2Y (dragDelta drag))) / realToFrac (max 1 (rectH (respRect resp) - 24)) + realToFrac wheel * 0.02 + keyboard
      next
        | respRightClicked resp = paramDefault Gain
        | delta /= 0 = fromNorm Gain (toNorm Gain base + delta)
        | otherwise = base
  pure (resp, next)
