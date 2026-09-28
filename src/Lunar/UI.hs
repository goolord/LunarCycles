-- | The LunarCycles window: transport along the top, and below it a grid of
-- panes the user can drag, resize, swap and maximize: the ring, the
-- timeline, the track list, the editor for the selected track (its rhythm,
-- functions and sound on tabs) and the Tidal code.
module Lunar.UI
  ( AppEnv (..)
  , newAppEnv
  , lunarApp
  , lunarView
  ) where

import Control.Exception (SomeException, try)
import Control.Monad
import Data.IORef
import Data.List (elemIndex, find, findIndex)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Lunar.Catalog
import Lunar.Codegen
import Lunar.Compile
import Lunar.Engine
import Lunar.Midi (exportMidi)
import Lunar.Model
import Lunar.Refactor (miniToSteps)
import Lunar.UI.Chain (transformChain)
import Lunar.UI.Code (codeView)
import Lunar.UI.Control (selectField, wrappedText)
import Lunar.UI.Editors
import Lunar.UI.Knob
import Lunar.UI.Layout
import Lunar.UI.Palette
import Lunar.UI.Ring (ring)
import Lunar.UI.Timeline
import NanoUI

data AppEnv = AppEnv
  { envEngine :: !Engine
  , envMemo :: !(IORef (Maybe (Song, Derived)))
  , envSoundFont :: !(Maybe FilePath)
  , envLayout :: !(Maybe GridNode)
    -- ^ The pane arrangement saved last time; 'Nothing' for the default.
  }

-- | What the view needs from a song, worked out once per change.
data Derived = Derived
  { dCompiled :: [Compiled]
  , dCode :: [(Maybe Int, Line)]
  , dCodeText :: Text
  }

newAppEnv :: Engine -> IO AppEnv
newAppEnv eng = do
  AppEnv eng <$> newIORef Nothing <*> soundFont <*> loadLayout

-- | Compile the song when it changed, and hand the engine the tracks that
-- sound.
derive :: AppEnv -> Song -> IO Derived
derive env song = do
  memo <- readIORef (envMemo env)
  case memo of
    Just (s, d) | s == song -> pure d
    _ -> do
      let compiled = compileSong song
          d = Derived compiled (songCodeTagged song) (songCodeText song)
      when (fmap (songCps . fst) memo /= Just (songCps song)) $ setCps (envEngine env) (songCps song)
      setTracks (envEngine env) (audible compiled)
      writeIORef (envMemo env) (Just (song, d))
      pure d

-- | Edits made by widgets during a frame, applied together at its end.
type Edit = (Song -> Song) -> NanoUI ()

lunarApp :: AppEnv -> NanoUI ()
lunarApp = void . lunarView

-- | What each pane of the grid shows. The pane ids are fixed, so a pane
-- keeps its content wherever it is dragged.
data PaneKind = PaneRing | PaneTimeline | PaneTracks | PaneEditor | PaneCode
  deriving (Eq, Show, Enum, Bounded)

paneId :: PaneKind -> Word64
paneId k = fromIntegral (fromEnum k) + 1

paneKind :: Word64 -> Maybe PaneKind
paneKind i = find ((== i) . paneId) [minBound .. maxBound]

paneTitle :: PaneKind -> Text
paneTitle = \case
  PaneRing -> "Cycle"
  PaneTimeline -> "Timeline"
  PaneTracks -> "Tracks"
  PaneEditor -> "Editor"
  PaneCode -> "Code"

-- | A listening column beside the score and its editing desk.
initialLayout :: GridNode
initialLayout =
  Split 10 AxisV 0.28
    (Split 11 AxisH 0.52 (pane PaneRing) (pane PaneTracks))
    (Split 12 AxisH 0.44 (pane PaneTimeline) (Split 13 AxisV 0.60 (pane PaneEditor) (pane PaneCode)))
  where
    pane = Pane . paneId

data EditorTab = TabRhythm | TabFunctions | TabSound
  deriving (Eq, Show)

-- | The whole window, returning the song as this frame left it.
lunarView :: AppEnv -> NanoUI Song
lunarView env = styled (const lunarTheme) $ do
  (song, setSong) <- useState demoSong
  pending <- liftIO (newIORef id)
  let edit :: Edit
      edit f = liftIO (modifyIORef' pending (f .))
      eng = envEngine env
  derived <- liftIO (derive env song)
  wasPlaying <- liftIO (enginePlaying eng)
  whenM (keyPressedOnce KeySpace) $ do
    liftIO (setPlaying eng (not wasPlaying))
    requestFrame
    wakeAfter 0
  now <- liftIO (engineCycle eng)
  playing <- liftIO (enginePlaying eng)
  when playing (wakeAfter (1 / 60))
  th <- uiTheme
  ww <- windowWidth
  (pageCycles, setPageCycles) <- useState (4 :: Int)
  (selectedId, setSelectedId) <- useState (Nothing :: Maybe Int)
  (editorTab, setEditorTab) <- useState TabRhythm
  (compactPane, setCompactPane) <- useState PaneEditor
  (arrangement, setArrangement) <- useState (fromMaybe initialLayout (envLayout env))
  let compiled = dCompiled derived
      tracks = songTracks song
      selected = case selectedId of
        Just i | any ((== i) . trackId) tracks -> Just i
        _ -> trackId <$> take1 tracks
      take1 = foldr (const . Just) Nothing
      selectedIndex = selected >>= \i -> findIndex ((== i) . trackId) tracks
      selectTrack i = do
        when (Just i /= selectedId) (setSelectedId (Just i))
        when (ww < 1100) (setCompactPane PaneEditor)
      span' = fromIntegral pageCycles
      pageStart = fromIntegral (floor (now / span') :: Int) * span'
      cyc = fromIntegral (floor now :: Int)
      anySolo = any trackSolo tracks
      silenced t = trackMuted t || (anySolo && not (trackSolo t))
      lanesFor from to =
        [ Lane
            { laneName = trackName t
            , laneColor = trackColor th i
            , laneEvents = eventsIn from to (cPattern c)
            , lanePitched = isPitched (trackSound t)
            , laneSignal = pickSignal (cSignals c)
            , laneSilenced = silenced t
            }
        | (i, c) <- zip [0 ..] compiled
        , let t = cTrack c
        ]
      docked = ww >= 1100
      frame = paneFrame docked
      viewPane pid pctx = case paneKind pid of
        Nothing -> frame pctx "Empty" Nothing (muted "This pane has nothing in it. Close it with the × in its corner.")
        Just kind -> case kind of
          PaneRing -> frame pctx (paneTitle kind) Nothing $ do
            let Rect _ _ pw ph = pgcRect pctx
                beside = pw >= 320 && pw > ph * 1.25
                scoreView = do
                  score <- ring (songCps song) now playing selectedIndex (lanesFor cyc (cyc + 1))
                  when playing (keepAnimating score)
                legend = forM_ (zip [0 ..] tracks) $ \(i, t) -> do
                  r <- styled (buttonStyle (foreground (trackColor th i) . hoverBackground (styleBg (themeButton th)) . pressBackground (themeWindow th)) . subtle) $
                    buttonWith' (fontSize 12 . maxW (if beside then 100 else max 94 (pw - 66))) (trackName t)
                  tooltip r (trackName t)
                  when (respClicked r) (selectTrack (trackId t))
            if beside
              then rowWith (tight . fillW . fillH . gap 12) $ do
                scoreView
                styled (inputStyle (borderWidth 0 . background (styleBg (themePanel th)))) $
                  scrollWith (fixedW 110 . fillH) $ columnWith (tight . fillW . gap 6) $ do
                    wrappedText (fontMuted . fontSize 12) "One revolution, one cycle"
                    legend
              else columnWith (tight . fillW . fillH . gap 6) $ do
                scoreView
                labelWith (fontMuted . fontSize 12 . alignCenter . fillW) "One revolution, one cycle"
                rowWith (tight . fillW . wrap . gap 10 . lineGap 4 . alignCenter) legend
          PaneTimeline ->
            frame
              pctx
              (paneTitle kind)
              ( Just . (300,) $ do
                  let pages = [1, 2, 4, 8]
                  i <- selectField 116 (map (\p -> tshow p <> (if p == 1 then " cycle" else " cycles")) pages) (fromMaybe 2 (elemIndex pageCycles pages))
                  when (pages !! i /= pageCycles) (setPageCycles (pages !! i))
              )
              $ do
                r <- timeline pageStart span' now playing selectedIndex (lanesFor pageStart (pageStart + span'))
                mouse <- uiMousePos
                when (respClicked r) $
                  forM_ (laneAt (respRect r) (length tracks) mouse) $ \i -> selectTrack (trackId (tracks !! i))
          PaneTracks ->
            frame
              pctx
              (paneTitle kind)
              ( Just . (250,) $ whenM (buttonWith (fontSize 13 . alignMid . minH 36) "+ Track") $ do
                  let tid = nextTrackId song
                  edit (\s -> s {songTracks = songTracks s <> [newTrack tid ("track " <> tshow tid) "cp" (Steps (replicate 8 Nothing))]})
                  selectTrack tid
              )
              $ trackList th (rectW (pgcRect pctx)) selected selectTrack silenced edit compiled
          PaneEditor ->
            frame pctx (paneTitle kind) Nothing $
              case [(i, c) | (i, c) <- zip [0 ..] compiled, Just (trackId (cTrack c)) == selected] of
                (i, c) : _ -> trackEditor th i now editorTab setEditorTab (silenced (cTrack c)) edit c
                [] -> muted "Add a track to edit it here."
          PaneCode ->
            frame
              pctx
              (paneTitle kind)
              ( Just . (280,) $ do
                  (copied, setCopied) <- useFlag False
                  scope $ when copied $ labelWith (fontTone Success . fontSize 12) "Copied"
                  whenM (buttonWith (fontSize 13 . alignMid . minH 36) "Copy") (setCopied =<< setClipboard (dCodeText derived))
              )
              $ codeView ((,) <$> selected <*> (trackColor th <$> selectedIndex)) (dCode derived)
  columnWith (fillW . fillH . tight . gap 0) $ do
    resetLayout <- styled flatSurface $ toolbarView env song edit playing now derived
    when (ww < 1100) $ rowWith (padXY 0 4 . tight . fillW) $ do
      next <- tabBarConfigured defaultTabsConfig {tabsStyle = TabUnderline} compactPane
        [tab k (paneTitle k) () | k <- [PaneRing, PaneTimeline, PaneTracks, PaneEditor, PaneCode]]
      when (next /= compactPane) (setCompactPane next)
    -- A narrow window shows one pane, and leaves the docked arrangement
    -- as it was.
    gridResp <-
      paneGrid
        defaultPaneGridConfig
          { pgLayout = fillW . fillH . padAll 8
          , pgSpacing = 1
          , pgLeeway = 3
          , pgEdgeBand = 16
          , pgMinSize = 120
          , pgTree = Just (if docked then arrangement else Pane (paneId compactPane))
          , pgDividerColor = Just (themeWindow th)
          , pgFocusable = False
          , pgPreserveDragSize = True
          , pgViewPane = viewPane
          }
    if resetLayout
      then liftIO forgetLayout >> setArrangement initialLayout
      else when docked $ forM_ (pgrTree gridResp) $ \arranged -> do
        when (arranged /= arrangement) (setArrangement arranged)
        when (pgrCommitted gridResp) (liftIO (saveLayout arranged))
    rowWith (padXY 20 8 . tight . fillW . gap 16 . alignMid) $ do
      labelWith (fontMuted . fontSize 12) (if playing then "Playing" else "Stopped")
      labelWith (fontMuted . fontSize 12) (tshow (length tracks) <> " tracks")
      flex
      labelWith (fontMuted . fontSize 12) "Space: play / stop"
  f <- liftIO (readIORef pending)
  let song' = f song
  when (song' /= song) (setSong song')
  pure song'

-- | A pane's frame: a title bar to drag it by, the pane's own controls and
-- a maximize button, over its body. The controls come with the pane width
-- they need beside the title and maximize button.
paneFrame :: Bool -> PaneGridCtx -> Text -> Maybe (Float, NanoUI ()) -> NanoUI () -> NanoUI PaneView
paneFrame docked pctx title controls body = do
  let w = rectW (pgcRect pctx)
      titleBar = if docked then paneDragHandle pctx else rowWith
  panelWith (grow . padXY 16 12 . gap 12) $ do
    when (docked || isJust controls) $ titleBar (tight . gap 8 . fillW . alignMid . fixedH 36) $ do
      labelWith (fontMedium . fontSize 15 . alignMid) title
      flex
      -- A pane narrower than its controls need keeps only its title and
      -- maximize button, so the controls never spill over the next pane's
      -- title.
      forM_ controls $ \(room, items) ->
        when (w <= 0 || w >= room) $ rowWith (tight . gap 6 . alignMid) items
      when docked $ do
        maxResp <- styled subtle (buttonWith' (fixedWH 36 36 . alignMid) (if pgcMaximized pctx then "⤡" else "⤢"))
        tooltip maxResp (if pgcMaximized pctx then "Restore the layout" else "Fill the window with this pane")
        when (respClicked maxResp) (if pgcMaximized pctx then pgcRestore pctx else pgcMaximize pctx)
    columnWith (tight . fillW . fillH) body
  pure PaneView {pvTitle = title, pvDraggable = False}

-- | Transport and workspace backing sit below the instrument surfaces.
flatSurface :: Theme -> Theme
flatSurface th = panelStyle (background (themeWindow th) . borderWidth 0 . cornerRadius 0) th

-- | The parameter a lane traces: the filter sweep when there is one.
pickSignal :: [(Param, a)] -> Maybe (Param, a)
pickSignal sigs = case lookup Cutoff sigs of
  Just s -> Just (Cutoff, s)
  Nothing -> case sigs of
    s : _ -> Just s
    [] -> Nothing

-- | Transport, and a menu for where the sound goes. True when Reset layout
-- was pressed.
toolbarView :: AppEnv -> Song -> Edit -> Bool -> Double -> Derived -> NanoUI Bool
toolbarView env song edit playing now derived = do
  let eng = envEngine env
  (status, setStatus) <- useText ""
  (outputOpen, setOutputOpen) <- useFlag False
  outs <- liftIO (midiOutputs eng)
  sel <- liftIO (selectedMidi eng)
  dirt <- liftIO (dirtStatus eng)
  msg <- liftIO (engineMessage eng)
  ww <- windowWidth
  panelWith (fillW . padXY 20 12 . tight) $ rowWith (tight . gap 16 . fillW . alignMid . wrap . lineGap 10) $ do
    labelWith (fontSize 20 . fontMedium . alignMid) "LunarCycles"
    playResp <- styled primary $ buttonWith' (fixedWH 96 40 . alignMid) (if playing then "■ Stop" else "▶ Play")
    tooltip playResp "Space"
    when (respClicked playResp) $ do
      liftIO (setPlaying eng (not playing))
      requestFrame
      wakeAfter 0
    rowWith (tight . gap 6 . alignMid) $ do
      labelWith (fontMuted . fontSize 12 . alignMid) "Tempo"
      bpm <-
        numericInputConfigured
          defaultNumericInputConfig {nicMin = 30, nicMax = 300, nicDecimals = 1, nicStep = 1, nicLayout = alignMid (fixedW 88 defaultLayout)}
          (songCps song * 240)
      when (abs (bpm / 240 - songCps song) > 1e-6) $ edit (\s -> s {songCps = bpm / 240})
    when (ww >= 1100) $ labelWith (fontSize 12 . fixedW 90 . fontMuted . alignMid) ("Cycle " <> showNum (fromIntegral (floor (now * 100) :: Int) / 100))
    flex
    let dirtOn = dirt `elem` [DirtOn, DirtStarting]
        outputName = T.intercalate " + " ([fromMaybe "No MIDI" sel] <> ["SuperDirt" | dirtOn])
        menuWidth = min 420 (ww - 24)
    fm <- uiFontMetrics
    outLabel <- truncateTextUi fm (min 220 (ww - 168)) outputName
    outResp <- buttonWith' (fontSize 14 . padXY 14 10 . minH 40 . alignMid) ("Output: " <> outLabel <> "  ▾")
    when (respClicked outResp) (setOutputOpen (not outputOpen))
    (dismiss, _) <-
      popupWith outputOpen ((defaultPopupConfig (AnchorRect (respRect outResp))) {cfgPlacement = PlacementBelow, cfgOffset = 8}) (fixedW menuWidth) $
        columnWith (padAll 12 . tight . gap 12 . fillW) $ do
          wakeAfter 0.1
          rowWith (tight . fillW . gap 12) $ do
            labelWith (fontMedium . alignMid) "MIDI output"
            flex
            rescan <- buttonWith' (alignMid . minH 36 . padXY 12 8) "Rescan"
            tooltip rescan "List MIDI outputs again, after starting a synth"
            when (respClicked rescan) (liftIO (rescanMidi eng))
          let names = "No MIDI output" : map moName outs
              current = maybe 0 (\nm -> maybe 0 (+ 1) (elemIndex nm (map moName outs))) sel
          j <- selectField (menuWidth - 36) names current
          when (j /= current) $ do
            liftIO (selectMidi eng (if j == 0 then Nothing else Just (names !! j)))
            requestFrame
          fluid <- liftIO (fluidRunning eng)
          case envSoundFont env of
            Just sf | not fluid -> do
              r <- buttonWith' (minH 36 . padXY 12 8) "Start FluidSynth"
              tooltip r "Start a software synthesizer with the installed soundfont"
              when (respClicked r) (liftIO (startFluid eng sf))
            Just _ -> hint "FluidSynth is running."
            Nothing -> hint "No General MIDI soundfont found for FluidSynth. Choose a MIDI synth above."
          separator
          labelWith fontMedium "SuperDirt"
          rowWith (tight . gap 12 . fillW) $ do
            (dResp, on) <- toggleSwitchWith' alignMid dirtOn
            tooltip dResp "Send the patterns to SuperDirt on 127.0.0.1:57120"
            when (on /= dirtOn) (liftIO (setDirt eng on))
            wrappedText (fontMuted . fontSize 12 . alignMid) $ case dirt of
              DirtOn -> "Sending to 127.0.0.1:57120"
              DirtStarting -> "Starting Tidal's stream…"
              DirtFailed e -> e
              DirtOff -> "Off"
          separator
          labelWith fontMedium "MIDI file"
          rowWith (tight . gap 12 . fillW) $ do
            exportResp <- buttonWith' (alignMid . minH 36 . padXY 12 8) "Export .mid"
            wrappedText (fontMuted . fontSize 12 . alignMid) "16 cycles, rendered by Euterpea"
            when (respClicked exportResp) $ do
              r <- liftIO (try @SomeException (exportMidi "lunar-cycles.mid" (songCps song) 16 (audible (dCompiled derived))))
              setStatus (either (\e -> "Export failed: " <> tshow e) (const "Wrote lunar-cycles.mid") r)
    when (respClicked dismiss) (setOutputOpen False)
    reset <-
      if ww < 1100
        then pure False
        else do
          resetResp <- styled subtle (buttonWith' (fontSize 12 . alignMid . minH 36) "Reset layout")
          tooltip resetResp "Put the panes back where they started"
          pure (respClicked resetResp)
    let lineText = T.intercalate ". " (filter (not . T.null) [status, msg])
    scope $ unless (T.null lineText) $ wrappedText (fontMuted . fontSize 12 . maxW 220 . alignMid) lineText
    pure reset

-- | Every track on one line: its colour, name and pattern, and mute and
-- solo. Clicking a line opens the track in the editor.
trackList :: Theme -> Float -> Maybe Int -> (Int -> NanoUI ()) -> (Track -> Bool) -> Edit -> [Compiled] -> NanoUI ()
trackList th paneWidth selected selectTrack silenced edit compiled =
  scrollWith (fillW . fillH) $
    columnWith (padRight 4 . tight . gap 6 . fillW) $ do
      when (null compiled) $ hint "No tracks yet. Choose + Track to add a rhythm."
      forM_ (zip [0 ..] compiled) $ \(i, c) -> withKey (trackId (cTrack c)) $ do
        let t = cTrack c
            tid = trackId t
            col = trackColor th i
            isSel = selected == Just tid
            update f = edit (mapTrack tid f)
            surface =
              if isSel
                then panelStyle (background (lerpColor (themeWindow th) col 0.14) . borderWidth 1 . borderColor (withAlpha col 0.75) . cornerRadius 6)
                else panelStyle (background (themeWindow th) . borderWidth 1 . borderColor (themeSeparator th) . cornerRadius 6)
            -- The track's colour down the row's edge, the key that ties it to
            -- its ring, lane and code.
            edge = panelStyle (background colorTransparent . borderLeft 4 (if silenced t then withAlpha col 0.3 else col))
            toggle = toggleButtonWith' (fixedWH 32 30 . fontSize 12 . alignMid)
        (_, area) <- styled surface $ panelWith (fillW . tight) $ withCursorShape UiCursorPointer $ mouseArea (fillW . padLRTB 6 10 8 8) $
          styled edge $ panelWith (padLeft 14 . tight . gap 6 . fillW) $ do
              rowWith (tight . gap 8 . fillW . alignMid) $ do
                labelWith (fontMono . fontSize 12 . fontColor col . alignMid) ("d" <> tshow (cChannel c))
                nameResp <- styled subtle (buttonWith' (fontSize 15 . alignMid . minH 32 . maxW (max 40 (paneWidth - 194))) (trackName t))
                tooltip nameResp (trackName t)
                when (respClicked nameResp) (selectTrack tid)
                flex
                labelWith (fontMuted . fontMono . fontSize 12 . alignMid) (T.take 12 (trackSound t))
              rowWith (tight . gap 6 . fillW . alignMid) $ do
                (muteResp, muted') <- toggle Warning "M" (trackMuted t)
                tooltip muteResp (if trackMuted t then "Unmute" else "Mute")
                when (muted' /= trackMuted t) $ update (\tr -> tr {trackMuted = muted'})
                (soloResp, solo') <- toggle Accent "S" (trackSolo t)
                tooltip soloResp (if trackSolo t then "Stop soloing" else "Solo")
                when (solo' /= trackSolo t) $ update (\tr -> tr {trackSolo = solo'})
                void $ richTextWith (fillW . padLeft 4 . fontMono . fontSize 12 . alignMid . fontColor (if silenced t then themeMuted th else styleFg (themePanel th))) [inlineText (sourceMini t)]
        when (respClicked area) (selectTrack tid)

-- | The selected track: its name at the top, then tabs for its rhythm, the
-- functions it passes through, and its sound.
trackEditor :: Theme -> Int -> Double -> EditorTab -> (EditorTab -> NanoUI ()) -> Bool -> Edit -> Compiled -> NanoUI ()
trackEditor th index now editorTab setEditorTab silenced edit c = do
  let t = cTrack c
      tid = trackId t
      col = trackColor th index
      pitched = isPitched (trackSound t)
      update f = edit (mapTrack tid f)
      beat = now - fromIntegral (floor now :: Int)
  (note, setNote) <- useText ""
  columnWith (tight . gap 8 . fillW . fillH) $ do
    rowWith (tight . gap 8 . fillW . alignMid) $ do
      labelWith (fontMono . fontColor col . fontSize 14 . alignMid) ("d" <> tshow (cChannel c))
      name' <- textInputConfigured defaultTextInputConfig {ticLayout = alignMid (fillW (maxW 220 defaultLayout))} (trackName t)
      when (name' /= trackName t) $ update (\tr -> tr {trackName = name'})
      flex
      removeResp <- styled subtle (buttonWith' (fixedWH 36 36 . alignMid) "✕")
      tooltip removeResp "Remove track"
      when (respClicked removeResp) $ edit (\s -> s {songTracks = filter ((/= tid) . trackId) (songTracks s)})
    tab' <-
      tabBarConfigured
        defaultTabsConfig {tabsStyle = TabUnderline}
        editorTab
        [tab TabRhythm "Rhythm" (), tab TabFunctions "Functions" (), tab TabSound "Sound" ()]
    when (tab' /= editorTab) (setEditorTab tab')
    let expression = T.intercalate " $ " (map (T.concat . map tokText . transformCode . snd) (trackChain t) <> [sourceCode t])
    void $ richTextWith (fillW . fontMono . fontSize 13 . fontColor (if silenced then themeMuted th else col)) [inlineText expression]
    scrollWith (fillW . fillH) $ columnWith (padRight 10 . tight . gap 10 . fillW) $ case editorTab of
      TabRhythm -> do
        rowWith (tight . gap 10 . alignMid . fillW . wrap . lineGap 6) $ do
          let mode = sourceMode (trackSource t)
          mode' <-
            tabBarConfigured
              defaultTabsConfig {tabsStyle = TabSegmented}
              mode
              [tab ModeSteps "Steps" (), tab ModeEuclid "Euclid" (), tab ModeMini "Mini" ()]
          when (mode' /= mode) $
            case convertSource pitched mode' t of
              Right tr -> update (const tr) >> setNote ""
              Left why -> setNote why
          scope $ case (trackSource t, sourceRule t) of
            (Steps _, Just rule) -> wrappedText (fontMuted . fontSize 12 . alignMid) ("Simplified: " <> rule)
            _ -> pure ()
        scope $ unless (T.null note) $ wrappedText (fontTone Warning . fontSize 12) note
        case trackSource t of
          Steps xs -> do
            xs' <- stepGrid col pitched beat xs
            when (xs' /= xs) $ update (\tr -> tr {trackSource = Steps xs'})
            rowWith (tight . gap 6 . alignMid . wrap . lineGap 6) $ do
              labelWith (fontMuted . fontSize 12 . alignMid) "steps"
              let n = length xs
              whenM (buttonWith (fixedWH 32 32 . alignMid) "−") $ update (\tr -> tr {trackSource = Steps (resizeSteps (max 1 (n `div` 2)) xs)})
              labelWith (fontMono . fixedW 28 . alignMid) (tshow n)
              whenM (buttonWith (fixedWH 32 32 . alignMid) "+") $ update (\tr -> tr {trackSource = Steps (resizeSteps (min 32 (n * 2)) xs)})
              whenM (buttonWith (minH 32 . alignMid) "◀ shift") $ update (\tr -> tr {trackSource = Steps (rotateSteps 1 xs)})
              whenM (buttonWith (minH 32 . alignMid) "shift ▶") $ update (\tr -> tr {trackSource = Steps (rotateSteps (-1) xs)})
              whenM (buttonWith (minH 32 . alignMid) "clear") $ update (\tr -> tr {trackSource = Steps (map (const Nothing) xs)})
            hint "Drag to paint. Scroll to change a sample or note. With keyboard focus, use left/right to choose a step, Enter to toggle, and up/down to change its value."
          src@Euclid {} -> do
            src' <- euclidEditor col pitched beat src
            when (src' /= src) $ update (\tr -> tr {trackSource = src'})
            hint "Scroll over the ring to add or remove pulses; drag around it to rotate."
          Mini txt -> do
            (txt', toSteps) <- miniEditor pitched (cError c) txt
            when (txt' /= txt) $ update (\tr -> tr {trackSource = Mini txt'})
            case toSteps of
              Just (snd', xs) -> update (\tr -> tr {trackSource = Steps xs, trackSound = if pitched || T.null snd' then trackSound tr else snd'})
              Nothing -> pure ()
      TabFunctions -> do
        chain' <- transformChain col (sourceCode t) (trackChain t)
        when (chain' /= trackChain t) $ update (\tr -> tr {trackChain = chain'})
        hint "Functions read left to right as the code does: the first wraps all the others. Drag a block to reorder it, click it to change it."
      TabSound -> do
        rowWith (tight . gap 8 . alignMid) $ do
          labelWith (fontMuted . alignMid) "Sound"
          let options = map soundLabel catalog <> [trackSound t | trackSound t `notElem` catalogNames]
              idx = fromMaybe (length catalog) (elemIndex (trackSound t) catalogNames)
          i <- selectField 210 options idx
          when (i /= idx && i < length catalog) $ update (\tr -> tr {trackSound = soundName (catalog !! i)})
        knobRow col now t update c
        hint "Drag, scroll or use arrow keys to set a knob. Right-click to reset. Use the signal button to add modulation."

-- | The source as it appears at the end of the code's chain.
sourceCode :: Track -> Text
sourceCode t
  | isPitched (trackSound t) = "note \"" <> sourceMini t <> "\" # s \"" <> trackSound t <> "\""
  | otherwise = "s \"" <> sourceMini t <> "\""

-- | Rewrite a track's source for another editor. A mini-notation pattern
-- that is not a plain grid stays as it is.
convertSource :: Bool -> SourceMode -> Track -> Either Text Track
convertSource pitched mode t = case (mode, trackSource t) of
  (ModeMini, _) -> Right t {trackSource = Mini (sourceMini t)}
  (ModeSteps, Euclid k n r v) -> Right t {trackSource = Steps (euclidSteps k n r v)}
  (ModeSteps, Mini txt) -> case miniToSteps pitched txt of
    Right (snd', xs) -> Right t {trackSource = Steps xs, trackSound = if pitched || T.null snd' then trackSound t else snd'}
    Left why -> Left ("This pattern can't be a step grid: it " <> why <> ".")
  (ModeEuclid, Steps xs) -> Right t {trackSource = toEuclid xs}
  (ModeEuclid, Mini txt) -> case miniToSteps pitched txt of
    Right (snd', xs) -> Right t {trackSource = toEuclid xs, trackSound = if pitched || T.null snd' then trackSound t else snd'}
    Left why -> Left ("This pattern can't be a euclidean rhythm: it " <> why <> ".")
  _ -> Right t
  where
    toEuclid xs =
      let n = max 1 (length xs)
          k = length [() | Just _ <- xs]
          v = case [x | Just x <- xs] of
            x : _ -> x
            [] -> 0
          r = fromMaybe 0 (find (\rot -> euclidSteps k n rot v == xs) [0 .. n - 1])
       in Euclid (max 1 k) n r v

knobRow :: Color -> Double -> Track -> ((Track -> Track) -> NanoUI ()) -> Compiled -> NanoUI ()
knobRow col now t update c = rowWith (tight . gap 14 . wrap . lineGap 16) $
  forM_ allParams $ \p -> withKey (fromEnum p) $ columnWith (tight . gap 6 . fixedW 68 . alignCenter) $ do
    let setting = Map.findWithDefault (defaultSetting p) p (trackParams t)
        expr = paramExpr p setting
        sweep = case expr of
          PRange lo hi _ _ -> Just (toNorm p lo, toNorm p hi)
          PConst _ -> Nothing
        live = lookup p (cSignals c) >>= \sig -> sampleAt sig now
        store s' = update (\tr -> tr {trackParams = if paramActive p s' || psSignal s' /= SigNone then Map.insert p s' (trackParams tr) else Map.delete p (trackParams tr)})
    labelWith (fontSize 12 . fontMuted . alignCenter) (knobLabel p)
    (kResp, base') <- paramKnob col p (psBase setting) sweep live
    tooltip kResp (paramName p <> ": drag or scroll to set, right-click to reset")
    when (abs (base' - psBase setting) > 1e-9) $ store setting {psBase = base'}
    labelWith (fontSize 12 . fontMono . alignCenter) (formatParam p (fromMaybe (psBase setting) live))
    (open, setOpen) <- useFlag False
    let sigName = case psSignal setting of
          SigNone -> "off"
          s -> T.toLower (T.drop 3 (tshow s))
    modResp <- styled (if psSignal setting /= SigNone then tinted (const col) else id) $
      buttonWith' (fixedW 64 . minH 28 . fontSize 12 . alignMid) ("∿ " <> sigName)
    tooltip modResp "Modulate with a continuous signal"
    when (respClicked modResp) (setOpen (not open))
    (dismiss, edited) <-
      popupWith open ((defaultPopupConfig (AnchorRect (respRect modResp))) {cfgPlacement = PlacementBelow}) (fixedW 300) $
        columnWith (padAll 12 . tight . gap 12 . fillW) $ do
          labelWith fontMedium (paramName p <> " modulation")
          let signals = allSignals
              sigLabels = map (\s -> if s == SigNone then "none" else T.toLower (T.drop 3 (tshow s))) signals
          si <- rowWith (tight . gap 6 . alignMid) $ do
            labelWith (fixedW 56 . fontMuted . fontSize 12 . alignMid) "signal"
            selectField 160 sigLabels (fromMaybe 0 (elemIndex (psSignal setting) signals))
          depth <- rowWith (tight . gap 6 . alignMid) $ do
            labelWith (fixedW 56 . fontMuted . fontSize 12 . alignMid) "depth"
            d <- sliderWith (fixedW 130 . alignMid) 0 0.5 (realToFrac (psDepth setting))
            labelWith (fontMono . fontSize 12 . alignMid) (showNum (fromIntegral (round (d * 100) :: Int) / 100))
            pure (fromIntegral (round (d * 100) :: Int) / 100)
          let periods = [0.25, 0.5, 1, 2, 4, 8, 16] :: [Rational]
          pi' <- rowWith (tight . gap 6 . alignMid) $ do
            labelWith (fixedW 56 . fontMuted . fontSize 12 . alignMid) "period"
            selectField 160 (map (\q -> showRat q <> (if q == 1 then " cycle" else " cycles")) periods) (fromMaybe 4 (elemIndex (psPeriod setting) periods))
          hint "The signal sweeps depth either side of the knob."
          done <- button "Done"
          pure (setting {psSignal = signals !! si, psDepth = depth, psPeriod = periods !! pi'}, done)
    case edited of
      Just (s', done) -> do
        when (s' /= setting) (store s')
        when done (setOpen False)
      Nothing -> pure ()
    when (respClicked dismiss) (setOpen False)

-- | A short explanation under an editor, wrapped to the pane.
hint :: Text -> NanoUI ()
hint t = void (richTextWith (fillW . fontMuted . fontSize 12) [inlineText t])

knobLabel :: Param -> Text
knobLabel = \case
  Resonance -> "reso"
  p -> paramName p

tshow :: Show a => a -> Text
tshow = T.pack . show
