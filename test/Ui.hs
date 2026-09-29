-- | Drives the whole window headlessly with scripted pointer input and
-- checks what each gesture does to the song, its code and the panes.
module Main (main) where

import Control.Monad (unless, void)
import Data.IORef
import Data.List (find, sortOn)
import Data.Maybe (listToMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lunar.Codegen (codeText, songCode)
import Lunar.Engine (DirtStatus (..), dirtStatus, engineCycle, newEngine, shutdownEngine, enginePlaying)
import Lunar.Model
import Lunar.Project (loadSong, saveSong)
import Lunar.UI (AppEnv (..), Workspace (..), lunarView, newAppEnv, workspaceName)
import Lunar.UI.Editors (euclidEditor)
import Lunar.UI.Control (selectField)
import Lunar.UI.Palette (lunarTheme)
import Lunar.UI.Playlist (laneCount)
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Backend (FontMetrics (..))
import NanoUI.Testing
import NanoUI.Testing.Assert (withInput)
import NanoUI.Testing.Harness
import NanoUI.Shortcut (ctrl, key, shift)
import Lunar.UI.Layout (loadLayout)
import Lunar.UI.Layout qualified
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Environment (setEnv)
import System.Exit (exitFailure)

-- | A layout's splits and panes, without its ratios.
data Shape = V Shape Shape | H Shape Shape | P Int
  deriving (Eq, Show)

shape :: GridNode -> Shape
shape = \case
  Split _ AxisV _ a b -> V (shape a) (shape b)
  Split _ AxisH _ a b -> H (shape a) (shape b)
  Pane i -> P (fromIntegral i)

forgetSaved :: IO ()
forgetSaved = mapM_ (Lunar.UI.Layout.forgetLayout . workspaceName) [minBound .. maxBound :: Workspace]

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = unless ok $ do
        T.putStrLn ("FAIL: " <> name)
        modifyIORef failures (+ 1)
  -- Keep test layouts and audio-server startup out of the user's environment.
  tmp <- getTemporaryDirectory
  let config = tmp <> "/lunar-cycles-ui-test"
  createDirectoryIfMissing True config
  setEnv "XDG_CONFIG_HOME" config
  setEnv "XDG_DATA_HOME" config
  setEnv "LUNAR_CYCLES_DISABLE_AUDIO_AUTOSTART" "1"
  forgetSaved
  eng <- newEngine (songCps demoSong)
  env <- newAppEnv eng
  ctx <- newContext
  songRef <- newIORef demoSong
  let base = withInput 1600 980
      view = do
        s <- lunarView env
        liftIO (writeIORef songRef s)
      frame inp = void (runFrame ctx inp view)
      idle = mapM_ frame [base, base, base]
      spanOf :: Text -> IO (Maybe Rect)
      spanOf t = do
        ss <- collectTextSpans ctx
        pure ((\(r, _, _, _, _) -> r) <$> find (\(_, txt, _, _, _) -> txt == t) (sortOn (\(Rect _ y _ _, _, _, _, _) -> y) ss))
      click t = do
        r <- spanOf t
        case r of
          Just rect -> do
            let (p, rel) = clickPair base (spanCenter rect)
            mapM_ frame [p, rel]
            idle
            pure True
          Nothing -> check ("found " <> t) False >> pure False
      clickOverlay t = do
        ss <- collectOverlayTextSpans ctx base
        case find (\(_, txt, _, _, _) -> txt == t) ss of
          Just (r, _, _, _, _) -> do
            let (p, rel) = clickPair base (spanCenter r)
            mapM_ frame [p, rel]
            idle
          Nothing -> check ("overlay control " <> t) False
      dragFrames from to =
        let midway = V2 ((v2X from + v2X to) / 2) ((v2Y from + v2Y to) / 2)
         in [pressAt base from, holdAt base midway, holdAt base to, holdAt base to, releaseAt (holdAt base to)]
      drag from to = mapM_ frame (dragFrames from to) >> idle
      grooveTracks song = concatMap (sequenceTracks song) (take 1 (songSequences song))
      track name = find ((== name) . trackName) . grooveTracks <$> readIORef songRef
      gainOf = maybe 1 (psBase . Map.findWithDefault (defaultSetting Gain) Gain . trackParams)
  idle

  -- Selecting from the padding of a track row must work, not just its name.
  trackSpans <- collectTextSpans ctx
  let d2 = (\(r, _, _, _, _) -> r) <$> find (\(r, txt, _, _, _) -> txt == "d2" && rectX r < 450) trackSpans
  case d2 of
    Just r -> do
      let (p, rel) = clickPair base (V2 (rectX r - 5) (rectY r + rectH r / 2))
      mapM_ frame [p, rel]
      idle
      selected <- collectTextSpans ctx
      check "track padding selects the snare" $
        any (\(rect, txt, _, _, _) -> txt == "snare" && rectX rect > 450 && rectY rect > 420) selected
    Nothing -> check "found snare track row" False
  _ <- click "kick"
  _ <- click "M"
  check "track mute button still toggles independently" . maybe False trackMuted =<< track "kick"
  _ <- click "M"
  check "track mute button restores the track" . maybe False (not . trackMuted) =<< track "kick"

  -- The editor starts on the first track, on its Rhythm tab.
  mapM_ (\t -> spanOf t >>= check ("editor tab " <> t) . (/= Nothing)) ["Rhythm", "Functions", "Sound"]

  -- Resolve the canvas under the pointer instead of guessing its width from text.
  steps <- spanOf "steps"
  gridRect <- case steps of
    Just (Rect sx sy _ _) -> do
      frame base {inputMousePos = V2 (sx + 10) (sy - 26)}
      getPrevRect ctx =<< getHotId ctx
    Nothing -> pure Nothing
  case gridRect of
    Just (Rect sx sy sw sh) -> do
      let cw = sw / 16
          cell i = sx + cw * (fromIntegral (i :: Int) + 0.5)
          gy = sy + sh - 20
      drag (V2 (cell 1) gy) (V2 (cell 3) gy)
      kick <- track "kick"
      case trackSource <$> kick of
        Just (Steps xs) -> check ("painting fills steps 1-3: " <> T.pack (show xs)) (take 4 xs == [Just 0, Just 0, Just 0, Just 0])
        other -> check ("kick is still a grid: " <> T.pack (show other)) False
      mapM_ frame [keyInp KeyRight base, base, keyInp KeyEnter base, base]
      kickKeyed <- track "kick"
      check "keyboard toggles the selected step" $ case trackSource <$> kickKeyed of
        Just (Steps xs) -> xs !! 1 == Nothing
        _ -> False
      check "step keyboard input does not start transport" . not =<< enginePlaying eng
    _ -> check "found the kick's grid" False

  -- The Sound tab's gain knob rises when dragged upwards.
  _ <- click "Sound"
  gain <- spanOf "gain"
  case gain of
    Just (Rect gx gy gw gh) -> do
      before <- gainOf <$> track "kick"
      let at = V2 (gx + gw / 2) (gy + gh + 28)
      drag at (V2 (v2X at) (v2Y at - 40))
      after <- gainOf <$> track "kick"
      check ("gain knob rises: " <> T.pack (show (before, after))) (after > before + 0.2)
      mapM_ frame [keyInp KeyDown base, base]
      keyed <- gainOf <$> track "kick"
      check "gain knob responds to arrow keys" (keyed < after)
    Nothing -> check "found the gain knob" False

  -- Selecting the hats in the track list opens them in the editor, where
  -- dragging a function block past its neighbour swaps them.
  _ <- click "hats"
  _ <- click "Functions"
  degrade <- spanOf "degradeBy 0.25"
  every3 <- spanOf "every 3 rev"
  case (degrade, every3) of
    (Just r1, Just (Rect x2 y2 w2 h2)) -> do
      drag (spanCenter r1) (V2 (x2 + w2 + 12) (y2 + h2 / 2))
      hats <- track "hats"
      check ("drag reorders the chain: " <> T.pack (show (fmap trackChain hats))) $
        fmap (map snd . trackChain) hats == Just [Every 3 Rev, Degrade 0.25]
      code <- codeText . songCode PlaySequence 1 <$> readIORef songRef
      check "code follows the new order" ("d3 $ every 3 rev\n   $ degradeBy 0.25" `T.isInfixOf` code)
    _ -> check "found the hats' function blocks" False

  _ <- click "every 3 rev"
  clickOverlay "Move later"
  movedHats <- track "hats"
  check "function menu reorders without dragging" $
    fmap (map snd . trackChain) movedHats == Just [Degrade 0.25, Every 3 Rev]
  _ <- click "every 3 rev"
  clickOverlay "Move earlier"
  movedBack <- track "hats"
  check "function menu moves a wrapper earlier" $
    fmap (map snd . trackChain) movedBack == Just [Every 3 Rev, Degrade 0.25]

  -- The "+ fn" menu wraps the hats in another function.
  _ <- click "+ fn"
  items <- collectOverlayTextSpans ctx base
  case find (\(_, txt, _, _, _) -> txt == "palindrome") items of
    Just (ri, _, _, _, _) -> do
      let (p, rel) = clickPair base (spanCenter ri)
      mapM_ frame [p, rel]
      idle
      hats <- track "hats"
      check ("menu adds palindrome: " <> T.pack (show (fmap trackChain hats))) $
        fmap (map snd . take 1 . trackChain) hats == Just [Palindrome]
    Nothing -> check "menu lists palindrome" False

  -- Switching the hats to Mini writes their grid as mini-notation.
  _ <- click "Rhythm"
  _ <- click "Mini"
  hats <- track "hats"
  check ("Mini tab converts: " <> T.pack (show (fmap trackSource hats))) $
    case trackSource <$> hats of
      Just (Mini txt) -> "hh" `T.isInfixOf` txt
      _ -> False

  -- The window opens on the Pattern view. Dragging the Timeline pane by its
  -- title onto the Tracks pane swaps them.
  timelineTitle <- spanOf "Timeline"
  tracksTitle <- spanOf "Tracks"
  case (timelineTitle, tracksTitle) of
    (Just rl, Just rt) -> do
      let target = V2 (rectX rt + 180) ((rectY rt + 950) / 2)
      drag (spanCenter rl) target
      timelineAfter <- spanOf "Timeline"
      tracksAfter <- spanOf "Tracks"
      check ("panes swapped: " <> T.pack (show (rl, rt, timelineAfter, tracksAfter))) $
        case (timelineAfter, tracksAfter) of
          (Just l', Just t') -> abs (rectX l' - rectX rt) < 40 && abs (rectX t' - rectX rl) < 40
          _ -> False
    _ -> check "found the pane titles" False

  -- The swapped arrangement was saved for the Pattern view, and a new
  -- window opens with it.
  saved <- loadLayout "pattern"
  check ("saved layout has the Timeline where the Tracks were: " <> T.pack (show saved)) $
    fmap shape saved == Just (V (H (P 1) (P 2)) (H (P 3) (P 4)))
  env2 <- newAppEnv eng
  ctx2 <- newContext
  let frame2 inp = void (runFrame ctx2 inp (lunarView env2))
  mapM_ frame2 [base, base, base]
  spans2 <- collectTextSpans ctx2
  let titleX t = (\(Rect x _ _ _, _, _, _, _) -> x) <$> find (\(_, txt, _, _, _) -> txt == t) spans2
  check ("reopened with the saved layout: " <> T.pack (show (titleX "Timeline", titleX "Tracks"))) $
    maybe False (< 100) (titleX "Timeline") && maybe False (> 400) (titleX "Tracks")

  -- A narrow window focuses one instrument without overwriting the dock layout.
  let narrow = withInput 440 820
      narrowIdle = mapM_ frame2 [narrow, narrow, narrow]
      narrowClick text = do
        ss <- collectTextSpans ctx2
        case find (\(_, txt, _, _, _) -> txt == text) ss of
          Just (r, _, _, _, _) -> do
            let (p, rel) = clickPair narrow (spanCenter r)
            mapM_ frame2 [p, rel]
            narrowIdle
          Nothing -> check ("narrow control " <> text) False
  narrowIdle
  narrowClick "Tracks"
  narrowClick "+ Track"
  narrowSpans <- collectTextSpans ctx2
  check "adding a track in compact view opens its editor" (hasText "track 6" narrowSpans && hasText "Rhythm" narrowSpans)
  narrowClick "✕"
  narrowClick "Cycle"
  narrowSpans2 <- collectTextSpans ctx2
  check "compact cycle view renders the score" (hasText "One revolution, one cycle" narrowSpans2)
  savedNarrow <- loadLayout "pattern"
  check "compact navigation preserves the saved desktop layout" (savedNarrow == saved)
  mapM_ frame2 [base, base, base]
  restored <- collectTextSpans ctx2
  check "resizing back restores the Pattern view's four panes" $
    all (`hasText` restored) ["One revolution, one cycle", "Timeline", "+ Track", "Rhythm"] && not (any (`hasText` restored) ["+ Sequence", "Copy"])

  -- Reset layout puts the view's panes back, and the old arrangement stays gone.
  resetSpans <- collectTextSpans ctx2
  case spanRectOf "Reset layout" resetSpans of
    Just r -> do
      let (p, rel) = clickPair base (spanCenter r)
      mapM_ frame2 [p, rel, base, base, base]
      afterReset <- collectTextSpans ctx2
      let titleAt t = listToMaybe (spanXOf t afterReset)
      savedReset <- loadLayout "pattern"
      check ("Reset layout restores the default panes: " <> T.pack (show (titleAt "Tracks", titleAt "Timeline"))) $
        maybe False (< 100) (titleAt "Tracks") && maybe False (> 400) (titleAt "Timeline")
      check ("Reset layout saves the default arrangement: " <> T.pack (show savedReset)) $
        fmap shape savedReset `elem` [Nothing, Just (V (H (P 1) (P 3)) (H (P 2) (P 4)))]
    Nothing -> check "found Reset layout" False

  -- A saved arrangement that does not hold a view's panes is set aside.
  Lunar.UI.Layout.saveLayout "arrange" (Split 10 AxisV 0.5 (Pane 1) (Pane 2))
  envStale <- newAppEnv eng
  check "a stale arrangement is not used" (Map.notMember WsArrange (envLayouts envStale))
  Lunar.UI.Layout.forgetLayout "arrange"

  -- In a cramped arrangement every pane is drawn inside the rect the grid
  -- gave it, so each title still picks its pane up.
  let cramped = Split 10 AxisV 0.717 (Split 11 AxisH 0.4 (Split 12 AxisV 0.212 (Pane 2) (Pane 1)) (Pane 4)) (Pane 3)
  mapM_ (\(title, target) -> do
      Lunar.UI.Layout.saveLayout "pattern" cramped
      env3 <- newAppEnv eng
      ctx3 <- newContext
      let frame3 inp = void (runFrame ctx3 inp (void (lunarView env3)))
      mapM_ frame3 [base, base, base]
      ss <- collectTextSpans ctx3
      case spanRectOf title ss of
        Just r -> do
          mapM_ frame3 (dragFrames (spanCenter r) target <> [base, base])
          moved <- loadLayout "pattern"
          check ("dragging " <> title <> " in a cramped layout moves it: " <> T.pack (show moved)) (moved /= Just cramped)
        Nothing -> check ("found " <> title <> " in a cramped layout") False)
    [("Tracks", V2 1000 200), ("Editor", V2 1000 200), ("Cycle", V2 1350 500), ("Timeline", V2 1350 500)]
  mapM_ (Lunar.UI.Layout.forgetLayout . workspaceName) [minBound .. maxBound]

  -- Ctrl+1, 2 and 3 switch between the views, each with its own panes.
  -- Each pane is known by a control only it has, since the view tabs and
  -- drawings share some of the pane titles' words.
  let viewTitles = do
        ss <- collectTextSpans ctx
        pure
          [ title :: Text
          | (title, mark) <- [("Playlist", "+ Sequence"), ("Cycle", "One revolution, one cycle"), ("Timeline", "Timeline"), ("Tracks", "+ Track"), ("Editor", "Rhythm"), ("Code", "Copy")]
          , hasText mark ss
          ]
  mapM_ frame [chordInp (ctrl <> key '3') base, base, base]
  check "Ctrl+3 opens the Code view" . (== ["Tracks", "Editor", "Code"]) =<< viewTitles
  mapM_ frame [chordInp (ctrl <> key '1') base, base, base]
  check "Ctrl+1 opens the Arrange view" . (== ["Playlist", "Timeline", "Tracks"]) =<< viewTitles
  mapM_ frame [chordInp (ctrl <> key '2') base, base, base]
  check "Ctrl+2 returns to the Pattern view" . (== ["Cycle", "Timeline", "Tracks", "Editor"]) =<< viewTitles

  play <- spanOf "▶ Play"
  case play of
    Nothing -> check "found Play" False
    Just r -> do
      let (p, rel) = clickPair base (spanCenter r)
      mapM_ frame [p, rel]
      deadline <- getWakeAt ctx
      clock <- getMonotonicTime
      check "Play schedules an immediate frame without pointer movement" (deadline > 0 && deadline <= clock)
      frame base
      playingSpans <- collectTextSpans ctx
      check "next frame displays Stop" (hasText "■ Stop" playingSpans)
  check "play starts the engine" =<< enginePlaying eng
  check "headless UI test does not start SuperDirt" . (== DirtOff) =<< dirtStatus eng
  _ <- click "■ Stop"
  check "stop stops the engine" . not =<< enginePlaying eng

  toolbarSpans <- collectTextSpans ctx
  case find (\(_, txt, _, _, _) -> "Output:" `T.isPrefixOf` txt) toolbarSpans of
    Just (r, _, _, _, _) -> do
      let (p, rel) = clickPair base (spanCenter r)
      mapM_ frame [p, rel]
      idle
      outputSpans <- collectOverlayTextSpans ctx base
      check "Output opens its controls" (all (`hasText` outputSpans) ["Samples", "Choose folder…", "MIDI output", "Rescan", "SuperDirt"])
      -- What Rescan reports goes in the status bar, on the line it already has.
      clickOverlay "Rescan"
      statusSpans <- collectTextSpans ctx
      let rowOf t = (\(sr, _, _, _, _) -> rectY sr) <$> find (\(_, txt, _, _, _) -> t txt) statusSpans
      check ("the MIDI report sits in the status bar: " <> T.pack (show (rowOf ("MIDI outputs" `T.isSuffixOf`), rowOf (== "Stopped")))) $
        case (rowOf ("MIDI outputs" `T.isSuffixOf`), rowOf (== "Stopped")) of
          (Just y1, Just y2) -> abs (y1 - y2) < 2
          _ -> False
      mapM_ frame [keyInp KeyEscape base, base, base]
      closedSpans <- collectOverlayTextSpans ctx base
      check "Escape closes Output" (not (hasText "MIDI output" closedSpans))
    Nothing -> check "found Output button" False

  -- Capture belongs to the ring until release, even far outside its bounds.
  ringCtx <- newContext
  sourceRef <- newIORef (Euclid 3 8 2 0)
  let ringBase = withInput 500 300
      ringView = do
        (source, store) <- useState (Euclid 3 8 2 0)
        next <- euclidEditor (themeGreen lunarTheme) False 0 source
        whenChanged source next store
        liftIO (writeIORef sourceRef next)
      whenChanged old new store = unless (old == new) (store new)
      ringFrame i = void (runFrame ringCtx i ringView)
      rotation = readIORef sourceRef >>= \case
        Euclid _ _ r _ -> pure r
        _ -> fail "Euclid editor changed source type"
  mapM_ ringFrame [ringBase, ringBase, ringBase {inputMousePos = V2 58 70}]
  ringRect <- getPrevRect ringCtx =<< getHotId ringCtx
  case ringRect of
    Just r -> do
      let V2 cx cy = spanCenter r
          start = V2 cx (cy - rectH r * 0.4)
          right = V2 (cx + rectW r * 2) cy
          below = V2 cx (cy + rectH r * 2)
      mapM_ ringFrame [pressAt ringBase start, holdAt ringBase right]
      check "Euclid turns while pointer is outside the ring" . (== 0) =<< rotation
      ringFrame (holdAt ringBase below)
      check "Euclid keeps rotating outside the ring" . (== 6) =<< rotation
      mapM_ ringFrame [releaseAt (holdAt ringBase below), ringBase]
      mapM_ ringFrame [pressAt ringBase below, holdAt ringBase start, releaseAt (holdAt ringBase start)]
      check "a drag beginning elsewhere does not rotate Euclid" . (== 6) =<< rotation
    Nothing -> check "found Euclid canvas" False

  selectCtx <- newContext
  let longDevice = "Synth input port (266644:0), a device name much wider than its control"
      selectView = rowWith (tight . gap 12) $ do
        void (selectField 190 [longDevice, "No MIDI output"] 0)
        void (buttonWith' (alignMid . minH 36) "Rescan")
  mapM_ (\i -> void (runFrame selectCtx i selectView)) [base, base, base]
  selectSpans <- collectTextSpans selectCtx
  check "long select labels are cut short" $ any (\(_, txt, _, _, _) -> "..." `T.isSuffixOf` txt) selectSpans
  check "select label stays inside the select" $
    all (\(Rect x _ w _, txt, _, _, _) -> txt == "Rescan" || x + w <= 190) selectSpans

  -- The playlist, in a fresh window with the default panes.
  forgetSaved
  envP <- newAppEnv eng
  ctxP <- newContext
  songP <- newIORef demoSong
  let frameP inp = void (runFrame ctxP inp (lunarView envP >>= liftIO . writeIORef songP))
      idleP = mapM_ frameP [base, base, base]
      playlistOf = songPlaylist <$> readIORef songP
      clipOf cid = find ((== cid) . clipId) <$> playlistOf
      clickAtP pos = let (p, rel) = clickPair base pos in mapM_ frameP [p, rel] >> idleP
      chord c = mapM_ frameP [chordInp c base, base, base]
  idleP
  chord (ctrl <> key '1')
  -- Panes keep the rects their splits give them whatever they hold: the
  -- song's longer code does not push the dividers.
  let paneTops = do
        ss <- collectTextSpans ctxP
        pure [(t, rectY r) | t <- ["Playlist", "+ Track", "Timeline"], Just r <- [spanRectOf t ss]]
      clickTextP t = collectTextSpans ctxP >>= \ss -> case spanRectOf t ss of
        Just r -> clickAtP (spanCenter r)
        Nothing -> check ("found " <> t) False
  topsBefore <- paneTops
  clickTextP "Song"
  topsSong <- paneTops
  check ("the song's code leaves the panes where they were: " <> T.pack (show (topsBefore, topsSong))) (length topsBefore == 3 && topsSong == topsBefore)
  clickTextP "Sequence"
  titleP <- spanRectOf "Playlist" <$> collectTextSpans ctxP
  -- The grid is the widget under a point right of the sequence list.
  gridP <- case titleP of
    Just (Rect tx ty _ _) -> do
      frameP base {inputMousePos = V2 (tx + 320) (ty + 90)}
      getPrevRect ctxP =<< getHotId ctxP
    Nothing -> pure Nothing
  case gridP of
    Just (Rect gx gy gw gh) | gw > 400 -> do
      -- Lanes share the grid's height, so where a lane is depends on how
      -- many there are.
      let atIn lanes c l = V2 (gx + gw * c / 32) (gy + 22 + (gh - 24) / fromIntegral lanes * (l + 0.5))
          atLive c l = (\n -> atIn (n :: Int) c l) . laneCount gh <$> playlistOf
      -- A click on an empty lane places the sequence being edited.
      clickAtP =<< atLive 26.5 2
      fresh <- find ((== 2) . clipLane) <$> playlistOf
      check ("a click places groove at cycle 26: " <> T.pack (show fresh)) $
        fmap (\c -> (clipSequence c, clipStart c, clipCycles c)) fresh == Just (1, 26, 4)
      -- Dragging a clip moves it along and across lanes.
      from <- atLive 27.5 2
      to <- atLive 29.5 3
      mapM_ frameP (dragFrames from to) >> idleP
      moved <- playlistOf
      check ("a clip drags to cycle 28 on the next lane: " <> T.pack (show moved)) $
        any (\c -> clipLane c == 3 && clipStart c == 28) moved && not (any ((== 2) . clipLane) moved)
      -- A right-click removes it.
      (rp, rrel) <- rightClickPair base <$> atLive 29.5 3
      mapM_ frameP [rp, rrel] >> idleP
      check "right-click removes the clip" . (== songPlaylist demoSong) =<< playlistOf
      -- The intro stretches from its right edge.
      edgeFrom <- atLive 0 0
      edgeTo <- atLive 6 0
      mapM_ frameP (dragFrames (V2 (gx + gw * 4 / 32 - 3) (v2Y edgeFrom)) edgeTo) >> idleP
      check "dragging a clip's right edge stretches it" . (== Just 6) . fmap clipCycles =<< clipOf 1
      chord (ctrl <> key 'z')
      check "Ctrl+Z undoes the stretch" . (== Just 4) . fmap clipCycles =<< clipOf 1
      chord (ctrl <> shift <> key 'z')
      check "Ctrl+Shift+Z redoes it" . (== Just 6) . fmap clipCycles =<< clipOf 1
      -- Each gesture undoes on its own: the stretch, the removal, the move
      -- and the placement.
      mapM_ (const (chord (ctrl <> key 'z'))) [1 .. 3 :: Int]
      check "undo steps back one gesture at a time" . any ((== 2) . clipLane) =<< playlistOf
      chord (ctrl <> key 'z')
      check "undo returns to the demo playlist" . (== songPlaylist demoSong) =<< playlistOf
      -- Clicking a clip opens its sequence.
      clickAtP =<< atLive 1 0
      introSpans <- collectTextSpans ctxP
      check "clicking the intro clip edits the intro" (hasText "Looping intro" introSpans && hasText "2 of 5 tracks in intro" introSpans)
      -- A double-click opens its sequence in the Pattern view; Ctrl+1 comes back.
      introAt <- atLive 1 0
      let (dp, drel) = clickPair base introAt
      mapM_ frameP [dp, drel, dp, drel] >> idleP
      patternSpans <- collectTextSpans ctxP
      check "double-clicking a clip opens the Pattern view" (hasText "Rhythm" patternSpans && not (hasText "+ Sequence" patternSpans))
      chord (ctrl <> key '1')
      -- The ruler moves the playhead, and plays the song from there.
      clickAtP (V2 (gx + gw * 12.4 / 32) (gy + 10))
      cycleNow <- engineCycle eng
      check ("the ruler moves the playhead to cycle 12: " <> T.pack (show cycleNow)) (abs (cycleNow - 12) < 1e-9)
      songSpans <- collectTextSpans ctxP
      check "the ruler switches the transport to the song" (hasText "Song, 24 cycles" songSpans)
      case spanRectOf "Sequence" songSpans of
        Just r -> clickAtP (spanCenter r)
        Nothing -> check "found the Sequence switch" False
      sequenceSpans <- collectTextSpans ctxP
      check "the Sequence switch plays the edited sequence again" (hasText "Looping intro" sequenceSpans && not (hasText "Song, 24 cycles" sequenceSpans))
      check "switching the transport rewinds it" . (== 0) =<< engineCycle eng
      -- New asks before throwing away changes.
      clickAtP =<< atLive 30.5 1
      projectSpans <- collectTextSpans ctxP
      case spanRectOf "Project  ▾" projectSpans of
        Just r -> do
          clickAtP (spanCenter r)
          menu <- collectOverlayTextSpans ctxP base
          case (\(r', _, _, _, _) -> r') <$> find (\(_, txt, _, _, _) -> "New" `T.isPrefixOf` txt) menu of
            Just rn -> clickAtP (spanCenter rn)
            Nothing -> check "the Project menu lists New" False
          confirm <- collectOverlayTextSpans ctxP base
          check "New asks before discarding changes" (hasText "Discard" confirm)
          check "the song is kept until New is confirmed" . (/= blankSong) =<< readIORef songP
          case spanRectOf "Discard" confirm of
            Just rd -> clickAtP (spanCenter rd)
            Nothing -> pure ()
          check "confirming New starts a blank song" . (== blankSong) =<< readIORef songP
        Nothing -> check "found the Project menu" False
    other -> check ("found the playlist grid: " <> T.pack (show other)) False

  chord (ctrl <> key '2')
  clickTextP "✕"
  chord (ctrl <> key '4')
  emptyMixer <- collectTextSpans ctxP
  check "empty Mixer explains how to add a channel" (any (\(_, txt, _, _, _) -> "No channels to mix." `T.isPrefixOf` txt) emptyMixer)

  envM <- newAppEnv eng
  rawCtxM <- newContext
  let metrics = ctxFontMetrics rawCtxM
      ctxM = withFontMetrics rawCtxM metrics {fmAdvance = \c -> fmAdvance metrics c * 0.55}
  songM <- newIORef demoSong
  let frameM inp = void (runFrame ctxM inp (lunarView envM >>= liftIO . writeIORef songM))
      idleM = mapM_ frameM [base, base, base]
      chordM c = mapM_ frameM [chordInp c base, base, base]
      clickM t = do
        ss <- collectTextSpans ctxM
        case spanRectOf t ss of
          Just r -> let (p, rel) = clickPair base (spanCenter r) in mapM_ frameM [p, rel] >> idleM
          Nothing -> check ("Mixer control " <> t) False
      channelM name = find ((== name) . chanName) . songChannels <$> readIORef songM
      valueM name p = maybe (paramDefault p) (psBase . Map.findWithDefault (defaultSetting p) p . chanParams) <$> channelM name
      canvasBelow t dy = do
        ss <- collectTextSpans ctxM
        case spanRectOf t ss of
          Just r -> do
            frameM base {inputMousePos = V2 (rectX r + rectW r / 2) (rectY r + rectH r + dy)}
            getPrevRect ctxM =<< getHotId ctxM
          Nothing -> pure Nothing
      dragM r delta = let p = spanCenter r in mapM_ frameM (dragFrames p (V2 (v2X p) (v2Y p + delta))) >> idleM
  idleM
  clickM "Mixer"
  mixerSpans <- collectTextSpans ctxM
  check "Mixer tab opens faders and grouped channel controls" (all (`hasText` mixerSpans) ["Gain", "Pan", "Channel controls: kick", "Low-pass filter", "Effects", "Sample playback"])
  fader <- canvasBelow "Gain" 50
  case fader of
    Just r | rectH r > 150 -> do
      before <- readIORef songM
      gainBefore <- valueM "kick" Gain
      dragM r (-24)
      gainAfter <- valueM "kick" Gain
      check "Mixer fader changes channel gain" (gainAfter > gainBefore)
      chordM (ctrl <> key 'z')
      check "a fader drag is one undo step" . (== before) =<< readIORef songM
      chordM (ctrl <> shift <> key 'z')
      check "redo restores the mixer edit" . (== gainAfter) =<< valueM "kick" Gain
      -- Refocus the fader before keyboard input; shortcuts may move focus.
      let (p, rel) = clickPair base (spanCenter r)
      mapM_ frameM [p, rel, keyInp KeyDown base, base]
      check "fader arrow keys lower gain" . (< gainAfter) =<< valueM "kick" Gain
      let (rp, rr) = rightClickPair base (spanCenter r)
      mapM_ frameM [rp, rr] >> idleM
      check "right-click resets the fader to unity" . (== 1) =<< valueM "kick" Gain
      frameM base {inputMousePos = spanCenter r, inputScroll = V2 0 1}
      idleM
      check "scrolling over a fader raises gain" . (> 1) =<< valueM "kick" Gain
      dragM r (-600)
      check "fader clamps to its maximum" . (== 1.5) =<< valueM "kick" Gain
      dragM r 600
      check "fader reaches zero without going negative" . (== 0) =<< valueM "kick" Gain
      mapM_ frameM [rp, rr] >> idleM
    _ -> check "found the Mixer gain fader" False
  pan <- canvasBelow "Pan" 24
  case pan of
    Just r -> do
      dragM r (-20)
      check "Mixer pan changes the stereo position" . (> 0.5) =<< valueM "kick" Pan
    _ -> check "found Mixer pan" False
  clickM "M"
  check "Mixer mute changes the shared channel" . maybe False chanMuted =<< channelM "kick"
  clickM "M"
  clickM "S"
  soloSpans <- collectTextSpans ctxM
  check "Mixer solo identifies excluded channels" (hasText "Excluded" soloSpans)
  check "Mixer solo changes the shared channel" . maybe False chanSolo =<< channelM "kick"
  clickM "S"
  clickM "snare"
  selectedM <- collectTextSpans ctxM
  check "selecting a strip opens its effects" (hasText "Channel controls: snare" selectedM)
  mapM_ (\(caption, param, delta) -> do
      knobRect <- canvasBelow caption 24
      case knobRect of
        Just r -> do
          before <- valueM "snare" param
          dragM r delta
          after <- valueM "snare" param
          check ("Mixer edits " <> caption) (after /= before)
        _ -> check ("found Mixer " <> caption <> " knob") False)
    [("cutoff", Cutoff, 16), ("reso", Resonance, -16), ("room", Room, -16), ("shape", Shape, -16), ("speed", Speed, -16), ("begin", Begin, -16), ("end", End, 16)]
  editedM <- readIORef songM
  let snares = [t | sq <- songSequences editedM, t <- sequenceTracks editedM sq, trackName t == "snare"]
  check "Mixer edits are shared in every sequence without adding parts" $ case snares of
    t : ts -> all ((== trackParams t) . trackParams) ts && map seqParts (songSequences editedM) == map seqParts (songSequences demoSong)
    [] -> False
  check "Mixer effects reach generated Tidal code" ("# shape " `T.isInfixOf` codeText (songCode PlaySequence 1 editedM))
  savedM <- saveSong (config <> "/mixer.lunar") editedM
  case savedM of
    Right path -> check "Mixer edits survive save and reopen" . (== Right editedM) =<< loadSong path
    Left e -> check ("save Mixer song: " <> e) False
  clickM "∿ off"
  modulation <- collectOverlayTextSpans ctxM base
  check "Mixer signal button opens modulation controls" (all (`hasText` modulation) ["gain modulation", "signal", "depth", "period", "Done"])
  let clickOverlayM t = do
        ss <- collectOverlayTextSpans ctxM base
        case spanRectOf t ss of
          Just r -> let (p, rel) = clickPair base (spanCenter r) in mapM_ frameM [p, rel] >> idleM
          Nothing -> check ("Mixer modulation control " <> t) False
  clickOverlayM "none"
  clickOverlayM "sine"
  check "Mixer modulation updates the channel" . maybe False ((== Just SigSine) . fmap psSignal . Map.lookup Gain . chanParams) =<< channelM "snare"
  clickOverlayM "Done"
  gainLabels <- collectTextSpans ctxM
  case [r | (r, txt, _, _, _) <- gainLabels, txt == "Gain"] of
    _ : r : _ -> do
      frameM base {inputMousePos = V2 (rectX r + rectW r / 2) (rectY r + rectH r + 50)}
      target <- getPrevRect ctxM =<< getHotId ctxM
      case target of
        Just fr -> do
          before <- valueM "snare" Gain
          dragM fr (-12)
          check "modulated fader still edits its base" . (> before) =<< valueM "snare" Gain
          check "fader preserves its modulation signal" . maybe False ((== Just SigSine) . fmap psSignal . Map.lookup Gain . chanParams) =<< channelM "snare"
        Nothing -> check "found modulated fader" False
    _ -> check "found snare gain label" False
  clickM "∿ sine"
  mapM_ frameM [keyInp KeyEscape base, base, base]
  closedM <- collectOverlayTextSpans ctxM base
  check "Escape closes Mixer modulation" (not (hasText "gain modulation" closedM))

  let compactM = withInput 400 700
      idleCompactM = mapM_ frameM [compactM, compactM, compactM]
      clickCompactM t = do
        ss <- collectTextSpans ctxM
        case spanRectOf t ss of
          Just r -> let (p, rel) = clickPair compactM (spanCenter r) in mapM_ frameM [p, rel] >> idleCompactM
          Nothing -> check ("compact Mixer control " <> t) False
  mapM_ frameM [chordInp (ctrl <> key '4') compactM, compactM, compactM]
  compactMixer <- collectTextSpans ctxM
  check "Ctrl+4 opens Mixer in compact navigation" (hasText "Gain" compactMixer)
  check "compact Mixer pages channels to fit" (hasText "Next" compactMixer && hasText "1-2 / 5" compactMixer)
  clickCompactM "Next"
  secondBank <- collectTextSpans ctxM
  check "Next reveals the next bank of channels" (all (`hasText` secondBank) ["hats", "bass", "3-4 / 5"] && not (hasText "kick" secondBank))
  clickCompactM "Next"
  lastBank <- collectTextSpans ctxM
  check "Next reaches the final channel" (all (`hasText` lastBank) ["clap", "5-5 / 5"])
  clickCompactM "clap"
  pickedCompact <- collectTextSpans ctxM
  check "compact strip selection stays in Mixer" (hasText "Gain" pickedCompact && not (hasText "Rhythm" pickedCompact))
  mapM_ frameM [compactM {inputMousePos = V2 340 500, inputScroll = V2 0 30}, compactM, compactM]
  compactEffects <- collectTextSpans ctxM
  case spanRectOf "end" compactEffects of
    Just r -> do
      let at = V2 (rectX r + rectW r / 2) (rectY r + rectH r + 24)
      frameM compactM {inputMousePos = at}
      target <- getPrevRect ctxM =<< getHotId ctxM
      case target of
        Just kr -> do
          let p = spanCenter kr
              q = V2 (v2X p) (v2Y p + 16)
          mapM_ frameM [pressAt compactM p, holdAt compactM q, releaseAt (holdAt compactM q)]
          idleCompactM
          check "compact Mixer scrolls to working effect controls" . (< 1) =<< valueM "clap" End
        Nothing -> check "compact effect knob is reachable" False
    Nothing -> check "compact effects are reachable by scrolling" False
  mapM_ frameM [compactM {inputMousePos = V2 340 500, inputScroll = V2 0 (-30)}, compactM, compactM]
  clickCompactM "Previous"
  clickCompactM "Previous"
  firstBank <- collectTextSpans ctxM
  check "Previous returns to first bank" (hasText "kick" firstBank)
  mapM_ frameM [base, base, base]
  wideMixer <- collectTextSpans ctxM
  check "resizing Mixer restores desktop strips" (all (`hasText` wideMixer) ["kick", "snare", "hats", "bass", "clap"])
  check "compact selection carries into desktop effects" (hasText "Channel controls: clap" wideMixer)

  n <- readIORef failures
  shutdownEngine eng
  if n == 0 then T.putStrLn "ui: all gestures behaved" else exitFailure
