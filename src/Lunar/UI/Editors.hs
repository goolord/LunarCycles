-- | Editors for a track's rhythm source: a step grid, a euclidean ring, and
-- a mini-notation field. Each takes the source and returns it as edited
-- this frame.
module Lunar.UI.Editors
  ( stepGrid
  , euclidEditor
  , miniEditor
  , intField
  ) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Lunar.Model
import Lunar.Refactor (miniToSteps)
import Lunar.UI.Palette
import Lunar.UI.Control
import Lunar.UI.Timeline (monoFont, smallFont)
import NanoUI
import NanoUI.Path qualified as P

-- | Which cell of @n@ across a rect the x coordinate falls in.
cellAt :: Rect -> Int -> Float -> Maybe Int
cellAt (Rect x _ w _) n px
  | n <= 0 || px < x || px >= x + w = Nothing
  | otherwise = Just (min (n - 1) (floor ((px - x) / w * fromIntegral n)))

-- | A row of steps. Click a step to switch it, drag along the row to paint
-- the same change over the steps passed, and turn the wheel over a step to
-- pick its sample (drums) or note (pitched). @beat@ is where the playhead is
-- in the cycle, from 0 to 1.
stepGrid :: Color -> Bool -> Double -> [Maybe Int] -> NanoUI [Maybe Int]
stepGrid col pitched beat steps = do
  mouse <- uiMousePos
  (paint, setPaint) <- useState (Nothing :: Maybe (Maybe Int))
  (cursor, setCursor) <- useInt 0
  let n = length steps
  resp <-
    focusCanvas
      defaultCanvasConfig
        { canvasLayout = fillW (fixedH 76 defaultLayout)
        , canvasTrackPointer = True
        , canvasCursor = Just (\_ _ _ -> UiCursorPointer)
        }
      $ \r@(Rect x y w h) -> do
        cdc <- drawContext
        let th = cdcTheme cdc
            cw = w / fromIntegral (max 1 n)
            hoverCell = if rectContains r mouse then cellAt r n (v2X mouse) else Nothing
            current = floor (beat * fromIntegral n) :: Int
        drawRoundedRect r 6 (styleBg (themeInput th))
        mapM_
          ( \(i, s) -> do
              let cx = x + fromIntegral i * cw
                  shaded = odd (i `div` 4)
                  cell = Rect (cx + 2) (y + 23) (max 1 (cw - 4)) (h - 28)
              when shaded $ drawRect (Rect cx y cw h) (withAlpha (themeSeparator th) 0.45)
              when (cw >= 22 || i `mod` 4 == 0) $
                drawTextWith smallFont (V2 (cx + cw / 2) (y + 11)) AlignCenter AlignMiddle (T.pack (show (i + 1))) (themeMuted th)
              case s of
                Just v -> do
                  drawRoundedRect cell 3 (withAlpha col (if i == current then 1 else 0.8))
                  when (v /= 0 || pitched) $
                    drawTextWith monoFont (V2 (cx + cw / 2) (y + 23 + (h - 28) / 2)) AlignCenter AlignMiddle (T.pack (show v)) (themeOnAccent th)
                Nothing -> do
                  drawRoundedRect cell 3 (withAlpha (styleBg (themeButton th)) (if i == current then 1 else 0.7))
                  drawStrokeRoundedRect cell 3 1 (withAlpha (styleBorder (themeButton th)) 0.35)
              when (hoverCell == Just i) $ drawStrokeRoundedRect cell 4 1.5 (styleFg (themeInput th))
              when (cdcFocused cdc && min (n - 1) cursor == i) $ drawStrokeRoundedRect cell 3 2 (themeFocusRing th)
              when (i == current) $ drawRect (Rect (cx + 2) (y + h - 3) (cw - 4) 2) (themeAccent th)
          )
          (zip [0 :: Int ..] steps)
  inp <- askInput
  (_, wheel) <- useWheelDeltaOn resp
  key <- focusedKeys resp
  let rect = respRect resp
      cell = cellAt rect n (v2X mouse)
      pressedNow = respPressed resp && pressedIn MouseLeft inp
      startPaint = case cell of
        Just i | pressedNow -> Just (maybe (Just (lastValue steps)) (const Nothing) (steps !! i))
        _ -> Nothing
      activePaint = if respPressed resp then maybe paint Just startPaint else Nothing
      painted = case (activePaint, cell) of
        (Just v, Just i) -> replaceAt i v steps
        _ -> steps
      wheeled = case cell of
        Just i
          | wheel /= 0
          , Just v <- painted !! i ->
              replaceAt i (Just (clampStep pitched (v + signum (round (wheel * 4))))) painted
        _ -> painted
      cursor' = max 0 (min (n - 1) (cursor + fromEnum (key KeyRight) - fromEnum (key KeyLeft)))
      keyed
        | n == 0 = wheeled
        | key KeyEnter || key KeySpace = replaceAt cursor' (maybe (Just (lastValue steps)) (const Nothing) (wheeled !! cursor')) wheeled
        | key KeyUp || key KeyDown = replaceAt cursor' (Just (clampStep pitched (maybe 0 id (wheeled !! cursor') + fromEnum (key KeyUp) - fromEnum (key KeyDown)))) wheeled
        | otherwise = wheeled
  when (cursor' /= cursor) (setCursor cursor')
  when (activePaint /= paint) (setPaint activePaint)
  pure keyed
  where
    lastValue xs = case [v | Just v <- xs] of
      [] -> 0
      vs -> last vs

clampStep :: Bool -> Int -> Int
clampStep pitched v
  | pitched = max (-24) (min 36 v)
  | otherwise = max 0 (min 15 v)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i v xs = [if j == i then v else x | (j, x) <- zip [0 ..] xs]

-- | A euclidean rhythm as a ring of steps. Turn the wheel over it to add or
-- remove pulses, and drag around it to rotate the rhythm. The fields beside
-- it set pulses, steps, rotation and the sample or note.
euclidEditor :: Color -> Bool -> Double -> Source -> NanoUI Source
euclidEditor col pitched beat src = case src of
  Euclid k n r v -> rowWith (tight . gap 14 . alignMid . fillW . wrap . lineGap 12) $ do
    (grab, setGrab) <- useState (Nothing :: Maybe (Float, Int))
    mouse <- uiMousePos
    resp <-
      canvasConfigured
        defaultCanvasConfig
          { canvasLayout = alignMid (fixedWH 116 116 defaultLayout)
          , canvasTrackPointer = True
          , canvasCursor = Just (\_ _ _ -> UiCursorGrab)
          }
        $ \(Rect x y w h) -> do
          cdc <- drawContext
          let th = cdcTheme cdc
              c = V2 (x + w / 2) (y + h / 2)
              rad = min w h / 2 - 10
              hits = euclidSteps k n r v
              pos i = let a = fromIntegral i / fromIntegral (max 1 n) * 2 * pi - pi / 2 in V2 (v2X c + rad * cos a) (v2Y c + rad * sin a)
              hitIdx = [i | (i, Just _) <- zip [0 :: Int ..] hits]
              current = floor (beat * fromIntegral n) :: Int
          drawStrokeCircle c rad 1 (themeSeparator th)
          when (length hitIdx > 1) $
            drawPathWith P.NonZero (P.polygon (map pos hitIdx)) (P.Solid (withAlpha col 0.18))
          when (length hitIdx > 1) $
            drawStrokePath (P.polygon (map pos hitIdx)) 1.5 (withAlpha col 0.7)
          mapM_
            ( \(i, s) -> do
                let p = pos i
                case s of
                  Just _ -> drawCircle p (if i == current then 7 else 5.5) col
                  Nothing -> drawCircle p (if i == current then 4.5 else 3) (themeMuted th)
                when (i == current) $ drawStrokeCircle p 9 1.5 (themeAccent th)
            )
            (zip [0 :: Int ..] hits)
          drawTextWith smallFont c AlignCenter AlignMiddle (T.pack (show k <> "/" <> show n)) (styleFg (themePanel th))
    (_, wheel) <- useWheelDeltaOn resp
    drag <- useDrag2DOn resp
    let Rect rx ry rw rh = respRect resp
        centre = V2 (rx + rw / 2) (ry + rh / 2)
        angle = atan2 (v2Y mouse - v2Y centre) (v2X mouse - v2X centre)
        stepAngle = 2 * pi / fromIntegral (max 1 n)
        grab' = if dragActive drag then Just (maybe (angle, r) id grab) else Nothing
        rotated = case grab' of
          Just (a0, r0) ->
            let turned = round (wrapAngle (a0 - angle) / stepAngle) :: Int
             in (r0 + turned) `mod` max 1 n
          Nothing -> r
        k' = max 0 (min n (k + signum (round (wheel * 4))))
    when (grab' /= grab) (setGrab grab')
    column' $ do
      k2 <- intField "pulses" 0 (fromIntegral n) k'
      n2 <- intField "steps" 1 32 n
      r2 <- intField "rotate" 0 (fromIntegral (max 0 (n2 - 1))) rotated
      v2 <- intField (if pitched then "note" else "sample") (if pitched then -24 else 0) (if pitched then 36 else 15) v
      pure (Euclid (min k2 n2) n2 (r2 `mod` max 1 n2) v2)
  _ -> pure src
  where
    column' = columnWith (tight . gap 4 . alignMid)
    wrapAngle a
      | a > pi = a - 2 * pi
      | a < -pi = a + 2 * pi
      | otherwise = a

intField :: Text -> Double -> Double -> Int -> NanoUI Int
intField name lo hi v = rowWith (tight . gap 6 . alignMid) $ do
  labelWith (fixedW 64 . fontMuted . fontSize 12 . alignMid) name
  round
    <$> numericInputConfigured
      defaultNumericInputConfig {nicMin = lo, nicMax = hi, nicLayout = alignMid (fixedW 84 defaultLayout)}
      (fromIntegral v)

-- | Mini-notation typed as text. Returns the text, and the grid it reads as
-- when the user asks to turn it into steps.
miniEditor :: Bool -> Maybe Text -> Text -> NanoUI (Text, Maybe (Text, [Maybe Int]))
miniEditor pitched parseError txt = columnWith (tight . gap 4 . fillW) $ do
  (txt', toSteps) <- rowWith (tight . gap 8 . fillW . alignMid) $ do
    t <- textInputConfigured defaultTextInputConfig {ticLayout = alignMid (fillW (fontMono defaultLayout))} txt
    let asGrid = miniToSteps pitched t
    (clicked, _) <- withTooltip
      (disabledWhen (either (const True) (const False) asGrid) (buttonWith' (alignMid . minH 36) "To steps"))
      (wrappedText (fixedW 280 . fontSize 13) (either ("Cannot become steps: " <>) (const "Rewrite as a step grid") asGrid))
    pure (t, if respClicked clicked then either (const Nothing) Just asGrid else Nothing)
  case parseError of
    Just e -> wrappedText (fontDanger . fontSize 12) (T.takeWhile (/= '\n') e)
    Nothing -> wrappedText (fontMuted . fontSize 12) "Tidal mini-notation: ~ rest · [a b] group · a*2 faster · <a b> alternate · a(3,8) euclid · a? drop"
  pure (txt', toSteps)
