-- | The playlist: the song's sequences placed as clips on lanes over
-- cycles, the way a DAW's arrangement places patterns. Beside it, the list
-- of sequences to choose from. A click on an empty lane places the chosen
-- sequence, a clip drags along and across lanes and stretches from its
-- right edge, and the ruler above moves the playhead.
module Lunar.UI.Playlist
  ( ClipPreview
  , Playhead (..)
  , PlaylistResult (..)
  , playlistGrid
  , sequencePicker
  , laneCount
  ) where

import Control.Monad (forM_, unless, when)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Lunar.Model
import Lunar.UI.Palette (trackColor)
import Lunar.UI.Timeline (smallFont)
import NanoUI

-- | Where each channel with a part in a sequence has notes, from its cycle
-- 0, as onset and end in cycles: one row per part, with the channel's place
-- in the song for its colour.
type ClipPreview = [(Int, [(Double, Double)])]

-- | Where the transport is within the song, and whether it is moving.
data Playhead = Playhead {phCycle :: !Double, phMoving :: !Bool}

data PlaylistResult = PlaylistResult
  { prClips :: ![Clip]
    -- ^ The clips as this frame left them.
  , prPick :: !(Maybe Int)
    -- ^ The sequence of a clip that was picked up, to edit.
  , prOpen :: !(Maybe Int)
    -- ^ The sequence of a clip that was double-clicked, to open.
  , prSeek :: !(Maybe Double)
    -- ^ A cycle the ruler was pressed or dragged to.
  }

-- | What a press on the grid holds on to until it is released.
data Grab
  = GrabMove !Int !Double
    -- ^ A clip, and where along it the pointer took it, in cycles.
  | GrabResize !Int
  | GrabSeek !Int
  deriving (Eq, Show)

rulerH :: Float
rulerH = 22

-- | Lanes drawn in a grid this tall: every lane in use and a free one below
-- them, and as many more as fit about 52 pixels each, at least three.
laneCount :: Float -> [Clip] -> Int
laneCount h clips = min 16 (maximum [3, maximum (-1 : map clipLane clips) + 2, floor ((h - rulerH - 2) / 52)])

-- | How cycles and lanes map to the grid's rect.
data Geo = Geo {geoRect :: !Rect, geoStart :: !Double, geoSpan :: !Double, geoLanes :: !Int}

laneH :: Geo -> Float
laneH g = let Rect _ _ _ h = geoRect g in max 18 ((h - rulerH - 2) / fromIntegral (max 1 (geoLanes g)))

xOf :: Geo -> Double -> Float
xOf g c = let Rect x _ w _ = geoRect g in x + realToFrac ((c - geoStart g) / geoSpan g) * w

cycleAtX :: Geo -> Float -> Double
cycleAtX g px = let Rect x _ w _ = geoRect g in geoStart g + realToFrac ((px - x) / max 1 w) * geoSpan g

laneTop :: Geo -> Int -> Float
laneTop g i = let Rect _ y _ _ = geoRect g in y + rulerH + fromIntegral i * laneH g

laneAtY :: Geo -> Float -> Int
laneAtY g py = max 0 (min (geoLanes g - 1) (floor ((py - laneTop g 0) / laneH g)))

inRuler :: Geo -> V2 -> Bool
inRuler g (V2 _ py) = let Rect _ y _ _ = geoRect g in py < y + rulerH

clipRect :: Geo -> Clip -> Rect
clipRect g c =
  let x0 = xOf g (fromIntegral (clipStart c))
      x1 = xOf g (fromIntegral (clipEnd c))
   in Rect (x0 + 1) (laneTop g (clipLane c) + 2) (max 3 (x1 - x0 - 2)) (laneH g - 4)

-- | The clip under a point, the one drawn last first, and whether the point
-- is on its right edge, where dragging stretches it.
clipAt :: Geo -> [Clip] -> V2 -> Maybe (Clip, Bool)
clipAt g clips p@(V2 px _)
  | inRuler g p = Nothing
  | otherwise = case [c | c <- reverse clips, rectContains (clipRect g c) p] of
      c : _ -> let Rect x _ w _ = clipRect g c in Just (c, px >= x + w - min 8 (w / 3))
      [] -> Nothing

-- | The grid. @current@ is the sequence a click places; @spanCycles@ how
-- many cycles fit across it. The playhead shows while the song plays.
playlistGrid :: Int -> [Sequence] -> (Int -> ClipPreview) -> Int -> Maybe Playhead -> [Clip] -> NanoUI PlaylistResult
playlistGrid current sequences preview spanCycles playhead clips = do
  -- The lane count is held for the length of a drag, so a clip dragged
  -- into the free lane does not shrink the lanes under the pointer.
  (held, setHeld) <- useState (Nothing :: Maybe (Grab, Int))
  (selected, setSelected) <- useState (Nothing :: Maybe Int)
  -- The clip pressed last and when, to tell a double-click.
  (lastPress, setLastPress) <- useState (Nothing :: Maybe (Int, Double))
  (viewStart, setViewStart) <- useInt 0
  mouse <- uiMousePos
  let grab = fst <$> held
      span' = fromIntegral spanCycles :: Double
      lanesIn r = maybe (laneCount (rectH r) clips) snd held
      songEnd = fromIntegral (maximum (0 : map clipEnd clips)) :: Double
      -- A moving playhead turns the page when it runs off the grid.
      start = case playhead of
        Just (Playhead now True)
          | grab == Nothing && (now < fromIntegral viewStart || now >= fromIntegral viewStart + span') ->
              spanCycles * max 0 (floor (now / span'))
        _ -> viewStart
      startD = fromIntegral start
      nameOf sid = maybe "?" seqName (find ((== sid) . seqId) sequences)
      placing = maybe 4 seqCycles (find ((== current) . seqId) sequences)
      cursorAt g p
        | inRuler g p = UiCursorPointer
        | Just (_, True) <- clipAt g clips p = UiCursorEwResize
        | isJust (clipAt g clips p) = if isJust grab then UiCursorGrabbing else UiCursorGrab
        | otherwise = UiCursorPointer
  resp <-
    canvasConfigured
      defaultCanvasConfig
        { canvasLayout = fillW (fillH defaultLayout)
        , canvasTrackPointer = True
        , canvasCursor = Just (\_ r p -> cursorAt (Geo r startD span' (lanesIn r)) p)
        }
      $ \r@(Rect x y w h) -> do
        cdc <- drawContext
        let th = cdcTheme cdc
            lanes = lanesIn r
            g = Geo r startD span' lanes
            fg = styleFg (themePanel th)
            cap = styleBg (themeButton th)
            rim = styleBorder (themeButton th)
            pxPerCycle = w / realToFrac span'
            labelEvery = fromMaybe 64 (find (\k -> fromIntegral k * pxPerCycle >= 30) [1, 2, 4, 8, 16, 32 :: Int])
        drawRoundedRect r 4 (themeWindow th)
        forM_ [0 .. lanes - 1] $ \i ->
          when (odd i) $ drawRect (Rect x (laneTop g i) w (laneH g)) (withAlpha (themeSeparator th) 0.3)
        -- Cycle rules, stronger every four cycles, and the cycle numbers.
        forM_ [ceiling startD .. floor (startD + span') :: Int] $ \c -> do
          let cx = xOf g (fromIntegral c)
              fourth = c `mod` 4 == 0
          when (fourth || pxPerCycle >= 10) $
            drawRect (Rect cx (y + rulerH) (if fourth then 1.5 else 1) (h - rulerH - 2)) (withAlpha (themeSeparator th) (if fourth then 1 else 0.5))
          when (c `mod` labelEvery == 0 && cx + 18 < x + w) $
            drawTextWith smallFont (V2 (cx + 4) (y + 4)) AlignStart AlignTop (tshow c) (themeMuted th)
        drawRect (Rect x (y + rulerH - 1) w 1) (themeSeparator th)
        -- The song loops where its last clip ends.
        when (songEnd > startD && songEnd <= startD + span') $ do
          let ex = xOf g songEnd
          drawRect (Rect (ex - 1) (y + 2) 2 (h - 4)) (withAlpha (themeMuted th) 0.7)
          drawRect (Rect (ex - 8) (y + 2) 7 7) (withAlpha (themeMuted th) 0.7)
        withClip (Rect x (y + rulerH) w (h - rulerH)) $ do
          -- Where a click would place the chosen sequence.
          case (grab, clipAt g clips mouse) of
            (Nothing, Nothing)
              | rectContains r mouse && not (inRuler g mouse) -> do
                  let ghost = clipRect g (Clip 0 current (laneAtY g (v2Y mouse)) (max 0 (floor (cycleAtX g (v2X mouse)))) placing)
                  drawStrokeRoundedRect ghost 4 1 (withAlpha fg 0.35)
            _ -> pure ()
          forM_ clips $ \c -> do
            let cr@(Rect cx cy _ ch) = clipRect g c
                own = clipSequence c == current
                chosen = selected == Just (clipId c)
                rows = preview (clipSequence c)
                labelled = ch >= 30
                top = cy + (if labelled then 17 else 3)
                areaH = cy + ch - 3 - top
                rowH = if null rows then 0 else min 7 (areaH / fromIntegral (length rows))
                rowsTop = top + (areaH - rowH * fromIntegral (length rows)) / 2
                cycles = fromIntegral (clipCycles c)
            drawRoundedRect cr 4 (if own then lerpColor cap fg 0.12 else cap)
            -- A short clip draws its notes faintly behind its name.
            withClip (rectInflate (-2) cr) $ do
              forM_ (zip [0 :: Int ..] rows) $ \(k, (colour, evs)) ->
                forM_ evs $ \(s, e) -> when (s < cycles) $ do
                  let ex = xOf g (fromIntegral (clipStart c) + s)
                      ew = max 1.5 (realToFrac (min e cycles - s) * pxPerCycle - 1)
                  drawRect (Rect ex (rowsTop + fromIntegral k * rowH) ew (max 1 (rowH - 1))) (withAlpha (trackColor th colour) (if labelled then 0.9 else 0.4))
              drawTextWith smallFont (V2 (cx + 6) (cy + (if labelled then 2 else ch / 2 - 7))) AlignStart AlignTop (nameOf (clipSequence c)) fg
            drawStrokeRoundedRect cr 4 (if chosen then 2 else 1) (if chosen then fg else if own then withAlpha fg 0.6 else rim)
        forM_ playhead $ \(Playhead now _) -> when (now >= startD && now <= startD + span') $ do
          let px = xOf g now
          drawRect (Rect (px - 1) (y + 2) 2 (h - 4)) (themeAccent th)
          drawCircle (V2 px (y + rulerH - 6)) 4 (themeAccent th)
  inp <- askInput
  (_, wheel) <- useWheelDeltaOn resp
  let lanes = lanesIn (respRect resp)
      g = Geo (respRect resp) startD span' lanes
      at = cycleAtX g (v2X mouse)
      atCycle = max 0 (floor at) :: Int
      pressedNow = respPressed resp && pressedIn MouseLeft inp
      freshId = 1 + maximum (0 : map clipId clips)
      -- What a press takes hold of, and the clips with any clip it placed.
      pressed
        | not pressedNow = Nothing
        | inRuler g mouse = Just (GrabSeek atCycle, clips, Nothing)
        | Just (c, True) <- clipAt g clips mouse = Just (GrabResize (clipId c), clips, Just c)
        | Just (c, False) <- clipAt g clips mouse = Just (GrabMove (clipId c) (at - fromIntegral (clipStart c)), clips, Just c)
        | otherwise =
            let c = Clip freshId current (laneAtY g (v2Y mouse)) atCycle placing
             in Just (GrabMove freshId (at - fromIntegral atCycle), clips <> [c], Just c)
      grab' = if respPressed resp then maybe (grabAfter grab) (\(gr, _, _) -> Just gr) pressed else Nothing
      grabAfter = \case
        Just (GrabSeek _) -> Just (GrabSeek atCycle)
        other -> other
      base = maybe clips (\(_, cs, _) -> cs) pressed
      moved = case grab' of
        Just (GrabMove cid off) -> [if clipId c == cid then c {clipStart = max 0 (round (at - off)), clipLane = laneAtY g (v2Y mouse)} else c | c <- base]
        Just (GrabResize cid) -> [if clipId c == cid then c {clipCycles = max 1 (round (at - fromIntegral (clipStart c)))} else c | c <- base]
        _ -> base
      seek = case (grab', grab) of
        (Just (GrabSeek c), Just (GrabSeek c0)) | c == c0 && not pressedNow -> Nothing
        (Just (GrabSeek c), _) -> Just (fromIntegral c)
        _ -> Nothing
      pickedClip = pressed >>= \(_, _, c) -> c
  deleteKey <-
    if respHovered resp && isJust selected && grab' == Nothing
      then (||) <$> keyPressed KeyDelete <*> keyPressed KeyBackspace
      else pure False
  let rightHit = if respHovered resp && pressedIn MouseRight inp then fst <$> clipAt g moved mouse else Nothing
      doomed = [clipId c | Just c <- [rightHit]] <> [cid | deleteKey, Just cid <- [selected]]
      final = filter ((`notElem` doomed) . clipId) moved
      selected' = case pickedClip of
        Just c -> Just (clipId c)
        Nothing
          | maybe False (`elem` doomed) selected -> Nothing
          | otherwise -> selected
      notches = signum (round (wheel * 4)) :: Int
      start' = if notches /= 0 && grab' == Nothing then max 0 (start - notches * max 1 (spanCycles `div` 8)) else start
  clock <- uiTime
  let pickedAgain = case (pickedClip, lastPress) of
        (Just c, Just (cid, at0)) -> clipId c == cid && clipId c /= freshId && clock - at0 < 0.4
        _ -> False
  -- Placing a clip does not start a double-click.
  forM_ pickedClip $ \c -> setLastPress (if pickedAgain || clipId c == freshId then Nothing else Just (clipId c, clock))
  when (grab' /= grab) (setHeld ((,lanes) <$> grab'))
  when (selected' /= selected) (setSelected selected')
  when (start' /= viewStart) (setViewStart start')
  pure
    PlaylistResult
      { prClips = final
      , prPick = case pickedClip of
          Just c | clipId c /= freshId -> Just (clipSequence c)
          _ -> Nothing
      , prOpen = if pickedAgain then clipSequence <$> pickedClip else Nothing
      , prSeek = seek
      }

-- | The song's sequences, one row each. Choosing a row makes it the one the
-- editor shows and a click on the playlist places. The chosen row's "⋯"
-- opens its name, length, Duplicate and Delete.
sequencePicker :: Int -> Song -> (Int -> NanoUI ()) -> ((Song -> Song) -> NanoUI ()) -> NanoUI ()
sequencePicker current song pick edit = do
  th <- uiTheme
  let fg = styleFg (themePanel th)
  scrollWith (fixedW 172 . fillH) $ columnWith (padRight 6 . tight . gap 4 . fillW) $
    forM_ (songSequences song) $ \sq -> withKey (seqId sq) $ do
      let sid = seqId sq
          chosen = sid == current
          uses = length (filter ((== sid) . clipSequence) (songPlaylist song))
          surface =
            if chosen
              then buttonStyle (background (lerpColor (styleBg (themeButton th)) fg 0.12) . borderColor fg)
              else subtle
      rowWith (tight . gap 4 . fillW . alignMid) $ do
        r <- styled surface (buttonWith' (fillW . fontSize 13 . minH 32 . alignMid) (seqName sq))
        tooltip r (seqName sq <> ": " <> T.pack (show (length (seqParts sq))) <> " parts, " <> placedText uses)
        when (respClicked r) (pick sid)
        when chosen $ do
          (open, setOpen) <- useFlag False
          more <- buttonWith' (fixedWH 32 32 . alignMid) "⋯"
          tooltip more "Rename, resize, duplicate or delete this sequence"
          when (respClicked more) (setOpen (not open))
          (dismiss, done) <-
            popupWith open ((defaultPopupConfig (AnchorRect (respRect more))) {cfgPlacement = PlacementBelow}) (fixedW 300) $
              columnWith (padAll 12 . tight . gap 12 . fillW) $ do
                name' <- rowWith (tight . gap 6 . alignMid . fillW) $ do
                  labelWith (fixedW 56 . fontMuted . fontSize 12 . alignMid) "name"
                  textInputConfigured defaultTextInputConfig {ticLayout = alignMid (fillW defaultLayout)} (seqName sq)
                unless (name' == seqName sq || T.null (T.strip name')) $ edit (mapSequence sid (\s -> s {seqName = name'}))
                cycles' <- rowWith (tight . gap 6 . alignMid) $ do
                  labelWith (fixedW 56 . fontMuted . fontSize 12 . alignMid) "length"
                  v <-
                    numericInputConfigured
                      defaultNumericInputConfig {nicMin = 1, nicMax = 64, nicLayout = alignMid (fixedW 84 defaultLayout)}
                      (fromIntegral (seqCycles sq))
                  labelWith (fontMuted . fontSize 12 . alignMid) "cycles"
                  pure (round v)
                when (cycles' /= seqCycles sq) $ edit (mapSequence sid (\s -> s {seqCycles = cycles'}))
                labelWith (fontMuted . fontSize 12) ("New clips cover the length. " <> placedText uses <> ".")
                rowWith (tight . gap 8 . fillW) $ do
                  dup <- button "Duplicate"
                  when dup $ do
                    edit (fst . duplicateSequence sid)
                    pick (nextSequenceId song)
                  gone <- disabledWhen (length (songSequences song) <= 1) (styled destructive (button "Delete"))
                  when gone (edit (removeSequence sid))
                  flex
                  ok <- button "Done"
                  pure (dup || gone || ok)
          when (respClicked dismiss || done == Just True) (setOpen False)

placedText :: Int -> Text
placedText = \case
  0 -> "not in the playlist"
  1 -> "placed once"
  n -> "placed " <> tshow n <> " times"

tshow :: Show a => a -> Text
tshow = T.pack . show
