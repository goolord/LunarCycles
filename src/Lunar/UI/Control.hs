module Lunar.UI.Control
  ( focusCanvas
  , focusedKeys
  , selectField
  , fitText
  , wrappedText
  , popupSurface
  ) where

import Control.Monad (when, void)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI

-- The pinned select renderer does not clip its own label at the chevron.
-- Measure with the control's font before giving it display labels; indices
-- still refer to the original options, whose full text is in the tooltip.
selectField :: Float -> [Text] -> Int -> NanoUI Int
selectField width options current = do
  labels <- mapM (fitText 14 (width - 42)) options
  let full = case drop (max 0 current) options of
        t : _ -> t
        [] -> ""
  ((_, picked), _) <- withTooltip
    (selectWith' (fixedW width . minH 36 . fontSize 14 . alignMid) labels current)
    (wrappedText (fixedW 280 . fontSize 13) full)
  pure picked

fitText :: Float -> Float -> Text -> NanoUI Text
fitText size width text = do
  fm <- resolveFontUi size WeightNormal FontStyleNormal FontRegular
  measured <- lineWidthUi fm line
  ellipsis <- lineWidthUi fm "…"
  if measured <= width then pure line
    else if ellipsis > width then pure ""
    else shorten fm 0 (T.length line)
  where
    line = T.replace "\n" " " (T.replace "\r" " " text)
    shorten fm lo hi
      | hi - lo <= 1 = pure (T.take lo line <> "…")
      | otherwise = do
          let mid = (lo + hi) `div` 2
              candidate = T.take mid line <> "…"
          w <- lineWidthUi fm candidate
          if w <= width then shorten fm mid hi else shorten fm lo mid

wrappedText :: (Layout -> Layout) -> Text -> NanoUI ()
wrappedText layout text = void (richTextWith (layout . fillW) [inlineText text])

popupSurface :: Theme -> Theme
popupSurface th =
  let bg = colorRGBA 47 74 82 255
      edge = colorRGBA 139 169 174 255
   in (panelStyle (background bg . borderColor edge) . windowStyle (background bg . borderColor edge))
        th {themeShadow = colorRGBA 5 18 22 220}

focusCanvas :: CanvasConfig -> (Rect -> CanvasM ()) -> NanoUI Response
focusCanvas cfg draw = do
  (resp, ()) <- customWidget defaultCustomWidgetSpec
    { widgetLayout = canvasLayout cfg
    , widgetContent = canvasContent cfg
    , widgetTrackPointer = canvasTrackPointer cfg
    , widgetCursor = canvasCursor cfg
    , widgetFocusable = True
    , widgetDraw = \cdc rect -> runCanvasFor cdc $ do
        draw rect
        when (cdcFocused cdc) $
          drawStrokeRoundedRect (rectInflate (-1) rect) 4 2 (themeFocusRing (cdcTheme cdc))
    }
  when (respPressed resp) (requestFocus (respId resp))
  pure resp

focusedKeys :: Response -> NanoUI (Key -> Bool)
focusedKeys resp = do
  focused <- isFocused (respId resp)
  input <- askInput
  pure (\key -> focused && pressedOnceIn key input)
