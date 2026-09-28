-- | A track's transforms as a row of blocks read left to right as the code
-- is: @every 4 (fast 2) $ jux rev $ s "…"@. Dragging a block along the row
-- moves the function in the chain, and so rewrites the code; a click opens
-- the block's settings.
module Lunar.UI.Chain
  ( transformChain
  , kindLabel
  ) where

import Control.Monad (forM, when)
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe, isJust)
import Data.Ratio (approxRational)
import Data.Text (Text)
import Data.Text qualified as T
import Lunar.Codegen (Tok (..), transformCode)
import Lunar.Model
import Lunar.UI.Editors (intField)
import Lunar.UI.Control (selectField, wrappedText)
import NanoUI

-- | A transform written as code, for a block's face.
codeText :: Transform -> Text
codeText = T.concat . map tokText . transformCode

kindLabel :: TransformKind -> Text
kindLabel = codeText . defaultTransform

kindDescription :: TransformKind -> Text
kindDescription = \case
  KFast -> "Play the pattern faster, fitting more of it into each cycle."
  KSlow -> "Play the pattern slower, stretching it over more cycles."
  KRev -> "Reverse each cycle."
  KPalindrome -> "Play forwards, then backwards, on alternate cycles."
  KBrak -> "Squash every other cycle into its first half and shift it by a quarter: a breakbeat feel."
  KIter -> "Start each cycle one division later than the last, rotating through the pattern."
  KPly -> "Repeat every event, splitting it into equal parts."
  KRot -> "Shift the values one event along, keeping the rhythm."
  KDegrade -> "Drop events at random with this probability."
  KChop -> "Cut each sample into parts, played one after the other in the event's time."
  KStriate -> "Interleave parts of the samples across the cycle."
  KHurry -> "Speed up both the pattern and the playback of its samples."
  KJux -> "Play the pattern hard left, and the transformed pattern hard right."
  KEvery -> "Apply the function on every nth cycle, counting from cycle 0."
  KWhenmod -> "Apply the function when the cycle number modulo the first number is at least the second."
  KSometimes -> "Apply the function to about half of the events."
  KOff -> "Play a transformed copy of the pattern offset by a fraction of a cycle."

-- | The chain editor. Returns the chain as edited this frame.
transformChain :: Color -> Text -> [(Int, Transform)] -> NanoUI [(Int, Transform)]
transformChain col source chain = rowWith (tight . gap 6 . alignMid . fillW . wrap . lineGap 6) $ do
  (editing, setEditing) <- useState (Nothing :: Maybe Int)
  (adding, setAdding) <- useFlag False
  addResp <- buttonWith' (fontSize 13 . minH 36 . alignMid) "+ fn"
  when (respClicked addResp) (setAdding (not adding))
  tooltip addResp "Wrap the pattern in another function"
  (addDismiss, added) <-
    popup adding ((defaultPopupConfig (AnchorRect (respRect addResp))) {cfgPlacement = PlacementBelow}) $
      columnWith (tight . gap 0) $ do
        picks <- forM chainKinds $ \k -> do
          r <- menuItem' (kindLabel k)
          tooltipAt PlacementRight r (kindDescription k)
          pure (k, respClicked r)
        pure (lookup True [(c, k) | (k, c) <- picks])
  let freshId = 1 + maximum (0 : map fst chain)
      withAdded = case added of
        Just (Just k) -> (freshId, defaultTransform k) : chain
        _ -> chain
  when (isJust (added >>= id) || respClicked addDismiss) (setAdding False)
  -- Blocks are laid out from where they were drawn last frame. While a
  -- popup is open its presses are not drags, even over a block beneath it.
  (drawnAt, setDrawnAt) <- useState ([] :: [(Int, Rect)])
  moving <- useReorder (map fst withAdded) (if isJust editing || adding then [] else drawnAt)
  blocks <- forM [(tid, t) | tid <- reorderPreview moving, Just t <- [lookup tid withAdded]] $ \(tid, t) -> withKey tid $ do
    let dragged = reorderDragging moving == Just tid
        tint = if dragged then 0.24 else 0.14
    resp <-
      withCursorShape (if dragged then UiCursorGrabbing else UiCursorGrab) $
        styled (buttonStyle (background (withAlpha col tint) . hoverBackground (withAlpha col 0.22) . borderColor (withAlpha col 0.65))) $
          buttonWith' (fontMono . fontSize 13 . minH 36 . alignMid) (codeText t)
    tooltip resp (kindDescription (transformKind t) <> "  Drag to reorder, click to edit.")
    let open = editing == Just tid
    (dismiss, edited) <-
      popupWith open ((defaultPopupConfig (AnchorRect (respRect resp))) {cfgPlacement = PlacementBelow}) (fixedW 340) $
        columnWith (padAll 12 . tight . gap 12 . fillW) $ do
          wrappedText (fontMono . fontMedium) (codeText t)
          wrappedText (fontMuted . fontSize 12) (kindDescription (transformKind t))
          t' <- editTransform t
          shift <- rowWith (tight . gap 8) $ do
            let position = fromMaybe 0 (findIndex ((== tid) . fst) withAdded)
            earlier <- disabledWhen (position == 0) (button "Move earlier")
            later <- disabledWhen (position == length withAdded - 1) (button "Move later")
            pure (fromEnum later - fromEnum earlier)
          (remove, done) <- rowWith (tight . gap 8) $ do
            r <- styled destructive (button "Remove")
            flex
            ok <- button "Done"
            pure (r, ok)
          pure (t', remove, done, shift)
    let clicked = respClicked resp && not (reorderMoved moving)
    when clicked (setEditing (if open then Nothing else Just tid))
    let (t2, removed) = case edited of
          Just (t', r, _, _) -> (t', r)
          Nothing -> (t, False)
        shift = maybe 0 (\(_, _, _, move) -> move) edited
        closing = respClicked dismiss || maybe False (\(_, r, ok, move) -> r || ok || move /= 0) edited
    when (open && closing) (setEditing Nothing)
    labelWith (fontMono . fontMuted . alignMid) "$"
    pure (tid, (t2, removed, shift, respRect resp))
  wrappedText (fontMono . fontSize 13 . alignMid) source
  let drawn = [(tid, rect) | (tid, (_, _, _, rect)) <- blocks]
      kept = [(tid, t) | tid <- reorderOrder moving, Just (t, removed, _, _) <- [lookup tid blocks], not removed]
  when (drawn /= drawnAt) (setDrawnAt drawn)
  pure (foldl (\xs (tid, (_, _, direction, _)) -> moveBy tid direction xs) kept blocks)

moveBy :: Int -> Int -> [(Int, Transform)] -> [(Int, Transform)]
moveBy tid direction chain
  | direction == 0 = chain
  | otherwise = case findIndex ((== tid) . fst) chain of
      Nothing -> chain
      Just from ->
        let target = max 0 (min (length chain - 1) (from + direction))
            rest = filter ((/= tid) . fst) chain
         in take target rest <> [chain !! from] <> drop target rest

-- | The settings of one transform, and of the function inside it for the
-- higher-order ones.
editTransform :: Transform -> NanoUI Transform
editTransform = \case
  Fast r -> Fast <$> ratField "factor" r
  Slow r -> Slow <$> ratField "factor" r
  Hurry r -> Hurry <$> ratField "factor" r
  Iter n -> Iter <$> intField "divisions" 1 16 n
  Ply n -> Ply <$> intField "repeats" 1 16 n
  Rot n -> Rot <$> intField "shift" (-16) 16 n
  Chop n -> Chop <$> intField "parts" 1 32 n
  Striate n -> Striate <$> intField "parts" 1 32 n
  Degrade p -> rowWith (tight . gap 6 . alignMid) $ do
    labelWith (fixedW 64 . fontMuted . fontSize 12 . alignMid) "amount"
    p' <- sliderWith (fixedW 150 . alignMid) 0 1 (realToFrac p)
    labelWith (fontMono . fontSize 12 . alignMid) (T.pack (show (fromIntegral (round (p' * 100) :: Int) / 100 :: Double)))
    pure (Degrade (fromIntegral (round (p' * 100) :: Int) / 100))
  Jux t -> Jux <$> innerField t
  Every n t -> Every <$> intField "every" 1 16 n <*> innerField t
  Whenmod a b t -> Whenmod <$> intField "cycle" 2 32 a <*> intField "from" 1 31 b <*> innerField t
  Sometimes t -> Sometimes <$> innerField t
  Off r t -> Off <$> ratField "offset" r <*> innerField t
  other -> do
    labelWith (fontMuted . fontSize 12) "No settings."
    pure other

-- | The function a higher-order transform applies.
innerField :: Transform -> NanoUI Transform
innerField t = columnWith (padLeft 10 . tight . gap 6 . fillW) $ do
  i <- rowWith (tight . gap 6 . alignMid) $ do
    labelWith (fixedW 64 . fontMuted . fontSize 12 . alignMid) "applies"
    selectField 180 (map kindLabel simpleKinds) (fromMaybe 0 (elemIndex (transformKind t) simpleKinds))
  let chosen = simpleKinds !! i
      t' = if chosen == transformKind t then t else defaultTransform chosen
  editTransform t'

ratField :: Text -> Rational -> NanoUI Rational
ratField name r = rowWith (tight . gap 6 . alignMid) $ do
  labelWith (fixedW 64 . fontMuted . fontSize 12 . alignMid) name
  v <-
    numericInputConfigured
      defaultNumericInputConfig {nicMin = 0.0625, nicMax = 16, nicStep = 0.25, nicDecimals = 3, nicLayout = alignMid (fixedW 96 defaultLayout)}
      (fromRational r)
  pure (if abs (v - fromRational r) < 1e-6 then r else approxRational v 0.001)
