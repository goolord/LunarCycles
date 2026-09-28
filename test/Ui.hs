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
import Lunar.Codegen (songCodeText)
import Lunar.Engine (newEngine, shutdownEngine, enginePlaying)
import Lunar.Model
import Lunar.UI (lunarView, newAppEnv)
import Lunar.UI.Editors (euclidEditor)
import Lunar.UI.Control (selectField)
import Lunar.UI.Palette (lunarTheme)
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (withInput)
import NanoUI.Testing.Harness
import Lunar.UI.Layout (SavedLayout (..), loadLayout)
import Lunar.UI.Layout qualified
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Environment (setEnv)
import System.Exit (exitFailure)

-- | A layout's splits and panes, without its ratios.
data Shape = V Shape Shape | H Shape Shape | P Int
  deriving (Eq, Show)

shape :: SavedLayout -> Shape
shape = \case
  LSplit True _ a b -> V (shape a) (shape b)
  LSplit False _ a b -> H (shape a) (shape b)
  LPane i -> P (fromIntegral i)

forgetSaved :: IO ()
forgetSaved = Lunar.UI.Layout.forgetLayout

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = unless ok $ do
        T.putStrLn ("FAIL: " <> name)
        modifyIORef failures (+ 1)
  -- Saved layouts go to a scratch config directory, not the user's.
  tmp <- getTemporaryDirectory
  let config = tmp <> "/lunar-cycles-ui-test"
  createDirectoryIfMissing True config
  setEnv "XDG_CONFIG_HOME" config
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
      track name = find ((== name) . trackName) . songTracks <$> readIORef songRef
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
        any (\(rect, txt, _, _, _) -> txt == "snare" && rectX rect > 450 && rectY rect > 480) selected
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
      code <- songCodeText <$> readIORef songRef
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

  -- Dragging the Code pane by its title onto the Tracks pane swaps them.
  codeTitle <- spanOf "Code"
  tracksTitle <- spanOf "Tracks"
  case (codeTitle, tracksTitle) of
    (Just rc, Just rt) -> do
      let target = V2 (rectX rt + 180) ((rectY rt + 950) / 2)
      drag (spanCenter rc) target
      codeAfter <- spanOf "Code"
      tracksAfter <- spanOf "Tracks"
      check ("panes swapped: " <> T.pack (show (rc, rt, codeAfter, tracksAfter))) $
        case (codeAfter, tracksAfter) of
          (Just c', Just t') -> abs (rectX c' - rectX rt) < 40 && abs (rectX t' - rectX rc) < 40
          _ -> False
    _ -> check "found the pane titles" False

  -- The swapped arrangement was saved, and a new window opens with it.
  saved <- loadLayout
  check ("saved layout has Code where Tracks was: " <> T.pack (show saved)) $
    fmap shape saved == Just (V (H (P 1) (P 5)) (H (P 2) (V (P 4) (P 3))))
  env2 <- newAppEnv eng
  ctx2 <- newContext
  let frame2 inp = void (runFrame ctx2 inp (lunarView env2))
  mapM_ frame2 [base, base, base]
  spans2 <- collectTextSpans ctx2
  let titleX t = (\(Rect x _ _ _, _, _, _, _) -> x) <$> find (\(_, txt, _, _, _) -> txt == t) spans2
  check ("reopened with the saved layout: " <> T.pack (show (titleX "Code", titleX "Tracks"))) $
    maybe False (< 100) (titleX "Code") && maybe False (> 1000) (titleX "Tracks")

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
  check "adding a track in compact view opens its editor" (hasText "track 5" narrowSpans && hasText "Rhythm" narrowSpans)
  narrowClick "✕"
  narrowClick "Cycle"
  narrowSpans2 <- collectTextSpans ctx2
  check "compact cycle view renders the score" (hasText "One revolution, one cycle" narrowSpans2)
  savedNarrow <- loadLayout
  check "compact navigation preserves the saved desktop layout" (savedNarrow == saved)
  mapM_ frame2 [base, base, base]
  restored <- collectTextSpans ctx2
  check "resizing back restores all five instruments" (all (`hasText` restored) ["Cycle", "Timeline", "Tracks", "Editor", "Code"])

  -- Reset layout puts the panes back, and the old arrangement stays gone.
  resetSpans <- collectTextSpans ctx2
  case spanRectOf "Reset layout" resetSpans of
    Just r -> do
      let (p, rel) = clickPair base (spanCenter r)
      mapM_ frame2 [p, rel, base, base, base]
      afterReset <- collectTextSpans ctx2
      let titleAt t = listToMaybe (spanXOf t afterReset)
      savedReset <- loadLayout
      check ("Reset layout restores the default panes: " <> T.pack (show (titleAt "Tracks", titleAt "Code"))) $
        maybe False (< 100) (titleAt "Tracks") && maybe False (> 1000) (titleAt "Code")
      check ("Reset layout saves the default arrangement: " <> T.pack (show savedReset)) $
        fmap shape savedReset `elem` [Nothing, Just (V (H (P 1) (P 3)) (H (P 2) (V (P 4) (P 5))))]
    Nothing -> check "found Reset layout" False

  -- In a cramped arrangement every pane is drawn inside the rect the grid
  -- gave it, so each title still picks its pane up.
  let cramped = LSplit True 0.717 (LSplit False 0.4 (LSplit True 0.212 (LPane 2) (LPane 1)) (LSplit False 0.764 (LPane 4) (LPane 3))) (LPane 5)
  mapM_ (\(title, target) -> do
      Lunar.UI.Layout.saveLayout cramped
      env3 <- newAppEnv eng
      ctx3 <- newContext
      let frame3 inp = void (runFrame ctx3 inp (void (lunarView env3)))
      mapM_ frame3 [base, base, base]
      ss <- collectTextSpans ctx3
      case spanRectOf title ss of
        Just r -> do
          mapM_ frame3 (dragFrames (spanCenter r) target <> [base, base])
          moved <- loadLayout
          check ("dragging " <> title <> " in a cramped layout moves it: " <> T.pack (show moved)) (moved /= Just cramped)
        Nothing -> check ("found " <> title <> " in a cramped layout") False)
    [("Tracks", V2 1000 200), ("Editor", V2 1000 200), ("Cycle", V2 1350 500), ("Timeline", V2 1350 500)]

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
  _ <- click "■ Stop"
  check "stop stops the engine" . not =<< enginePlaying eng

  toolbarSpans <- collectTextSpans ctx
  case find (\(_, txt, _, _, _) -> "Output:" `T.isPrefixOf` txt) toolbarSpans of
    Just (r, _, _, _, _) -> do
      let (p, rel) = clickPair base (spanCenter r)
      mapM_ frame [p, rel]
      idle
      outputSpans <- collectOverlayTextSpans ctx base
      check "Output opens its controls" (all (`hasText` outputSpans) ["MIDI output", "Rescan", "SuperDirt", "Export .mid"])
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
  check "long select labels are ellipsized" $ any (\(_, txt, _, _, _) -> "…" `T.isSuffixOf` txt && txt /= longDevice) selectSpans
  check "select label stays short of its chevron and neighbour" $
    all (\(Rect x _ w _, txt, _, _, _) -> txt == "Rescan" || x + w <= 190 - 20) selectSpans

  n <- readIORef failures
  shutdownEngine eng
  if n == 0 then T.putStrLn "ui: all gestures behaved" else exitFailure
