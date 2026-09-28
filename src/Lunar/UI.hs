-- | The LunarCycles window: transport along the top, tabs for its three
-- views, and below them the view's grid of panes, which the user can drag,
-- resize, swap and maximize. Arrange centres on the playlist, Pattern on the
-- editor for the selected track (its rhythm, functions and sound on tabs),
-- and Code on the Tidal code; the ring, timeline and track list appear
-- where they serve the view.
module Lunar.UI
  ( AppEnv (..)
  , Workspace (..)
  , workspaceName
  , newAppEnv
  , lunarApp
  , lunarView
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad
import Data.Char (isAlphaNum)
import Data.IORef
import Data.List (elemIndex, find, findIndex, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Lunar.Catalog
import Lunar.Codegen
import Lunar.Compile
import Lunar.Engine
import Lunar.Midi (exportMidi)
import Lunar.Model
import Lunar.Project
import Lunar.Refactor (miniToSteps)
import Lunar.UI.Chain (transformChain)
import Lunar.UI.Code (codeView)
import Lunar.UI.Control (selectField, wrappedText)
import Lunar.UI.Editors
import Lunar.UI.Knob
import Lunar.UI.Layout
import Lunar.UI.Palette
import Lunar.UI.Playlist
import Lunar.UI.Ring (ring)
import Lunar.UI.Timeline
import NanoUI
import NanoUI.Shortcut (ctrl, key, shift)
import NanoUI.Backend.Sdl
  ( FileDialogId
  , FileDialogOptions (..)
  , FileDialogResult (..)
  , FileFilter (..)
  , askOpenFileDialog
  , askOpenFolderDialog
  , askSaveFileDialog
  , defaultFileDialogOptions
  , pollFileDialogUi
  )
import System.FilePath (takeExtension, (<.>))

data AppEnv = AppEnv
  { envEngine :: !Engine
  , envMemo :: !(IORef (Maybe ((Song, PlayMode, Int), Derived)))
  , envSoundFont :: !(Maybe FilePath)
  , envLayouts :: !(Map.Map Workspace GridNode)
    -- ^ Each view's pane arrangement as saved last time, where there is one.
  , envStartView :: !Workspace
  }

-- | What the view needs from a song, worked out once per change.
data Derived = Derived
  { dCompiled :: [Compiled]
    -- ^ The edited sequence's tracks, as the transport plays them.
  , dAudible :: [Compiled]
    -- ^ Every track the engine plays.
  , dCode :: CodeLines
  , dCodeText :: Text
  , dPreviews :: Map.Map Int ClipPreview
  }

-- | The engine's environment, with each view's saved arrangement. The
-- window opens on the Pattern view.
newAppEnv :: Engine -> IO AppEnv
newAppEnv eng = do
  saved <- forM [minBound .. maxBound] $ \w -> fmap ((,) w) <$> loadLayout (workspaceName w)
  let fits (w, t) = panesOf t == panesOf (startingLayout w)
  AppEnv eng
    <$> newIORef Nothing
    <*> soundFont
    <*> pure (Map.fromList (filter fits [x | Just x <- saved]))
    <*> pure WsPattern

-- | Compile the song when it, the play mode or the edited sequence changed,
-- and hand the engine the tracks that sound.
derive :: AppEnv -> Song -> PlayMode -> Int -> IO Derived
derive env song mode current = do
  memo <- readIORef (envMemo env)
  case memo of
    Just (memoKey, d) | memoKey == (song, mode, current) -> pure d
    _ -> do
      let arranged = arrangeSong song
          own = maybe [] (compileSequence song) (findSequence current song)
          (compiled, sounding) = case mode of
            PlaySequence -> (own, audible own)
            PlaySong -> (fromMaybe [] (lookup current arranged), audible (byChannel (map snd arranged)))
          code = songCode mode current song
          previewCycles sq = maximum (seqCycles sq : [clipCycles c | c <- songPlaylist song, clipSequence c == seqId sq])
          preview sq =
            [ (i, evs)
            | (i, t, evs) <- zip3 [0 ..] (sequenceTracks song sq) (sequencePreview (previewCycles sq) (compileSequence song sq))
            , partsIn sq (trackId t)
            ]
          previews = Map.fromList [(seqId sq, preview sq) | sq <- songSequences song]
          d = Derived compiled sounding code (codeText code) previews
      when (fmap (\((s, _, _), _) -> songCps s) memo /= Just (songCps song)) $ setCps (envEngine env) (songCps song)
      setTracks (envEngine env) sounding
      writeIORef (envMemo env) (Just ((song, mode, current), d))
      pure d

-- | Edits made by widgets during a frame, applied together at its end.
type Edit = (Song -> Song) -> NanoUI ()

lunarApp :: AppEnv -> NanoUI ()
lunarApp = void . lunarView

-- | What each pane of the grid shows. The pane ids are fixed, so a pane
-- keeps its content wherever it is dragged.
data PaneKind = PaneRing | PaneTimeline | PaneTracks | PaneEditor | PaneCode | PanePlaylist
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
  PanePlaylist -> "Playlist"

-- | The window's views, each a grid of the panes one kind of work needs.
data Workspace = WsArrange | WsPattern | WsCode
  deriving (Eq, Ord, Show, Enum, Bounded)

workspaceTitle :: Workspace -> Text
workspaceTitle = \case
  WsArrange -> "Arrange"
  WsPattern -> "Pattern"
  WsCode -> "Code"

-- | The name a view goes by on the command line and in its layout file.
workspaceName :: Workspace -> String
workspaceName = T.unpack . T.toLower . workspaceTitle

-- | Each view as it first opens. Arrange gives the playlist the width and
-- most of the height, with the edited sequence's tracks and timeline under
-- it. Pattern is a listening column beside the timeline and the editor.
-- Code sets the Tidal code beside the editor, so a change and the code it
-- writes are seen together.
startingLayout :: Workspace -> GridNode
startingLayout = \case
  WsArrange ->
    Split 20 AxisH 0.60 (pane PanePlaylist) (Split 21 AxisV 0.28 (pane PaneTracks) (pane PaneTimeline))
  WsPattern ->
    Split 10 AxisV 0.27
      (Split 11 AxisH 0.50 (pane PaneRing) (pane PaneTracks))
      (Split 12 AxisH 0.36 (pane PaneTimeline) (pane PaneEditor))
  WsCode ->
    Split 30 AxisV 0.50 (pane PaneCode) (Split 31 AxisH 0.62 (pane PaneEditor) (pane PaneTracks))
  where
    pane = Pane . paneId

-- | The panes in an arrangement, in order.
panesOf :: GridNode -> [Word64]
panesOf = sort . go
  where
    go = \case
      Pane i -> [i]
      Split _ _ _ a b -> go a <> go b

data EditorTab = TabRhythm | TabFunctions | TabSound
  deriving (Eq, Show)

-- | Undo and redo. Edits made in one pointer drag, or typed or scrolled
-- within a second of each other, undo together.
data History = History
  { hPast :: ![Song]
  , hFuture :: ![Song]
  , hGroup :: !EditGroup
  , hAt :: !Double
  }
  deriving (Eq)

data EditGroup = GroupNone | GroupDrag | GroupTyping
  deriving (Eq)

emptyHistory :: History
emptyHistory = History [] [] GroupNone 0

data HistoryStep = StepUndo | StepRedo

-- | Where the song was last saved, to know whether it has changed since.
data Project = Project {projPath :: !(Maybe FilePath), projSaved :: !Song}
  deriving (Eq)

-- | What a file dialog was opened for.
data DialogFor = ForOpen | ForSave | ForExport
  deriving (Eq)

-- | The whole window, returning the song as this frame left it.
lunarView :: AppEnv -> NanoUI Song
lunarView env = styled (const lunarTheme) $ do
  (song, setSong) <- useState demoSong
  (history, setHistory) <- useState emptyHistory
  (project, setProject) <- useState (Project Nothing demoSong)
  (mode, setMode) <- useState PlaySequence
  (currentPick, setCurrent) <- useState (Nothing :: Maybe Int)
  (status, setStatus) <- useText ""
  (dialog, setDialog) <- useState (Nothing :: Maybe (DialogFor, FileDialogId))
  pending <- liftIO (newIORef id)
  stepRequest <- liftIO (newIORef Nothing)
  replacement <- liftIO (newIORef Nothing)
  let edit :: Edit
      edit f = liftIO (modifyIORef' pending (f .))
      eng = envEngine env
      sequences = songSequences song
      currentSeq =
        fromMaybe (newSequence 0 "sequence") $
          (currentPick >>= \i -> find ((== i) . seqId) sequences) <|> listToMaybe sequences
      current = seqId currentSeq
      editTrack tid f = edit (mapTrack current tid f)
      pickSequence sid = when (Just sid /= currentPick) (setCurrent (Just sid))
  derived <- liftIO (derive env song mode current)
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
  (playlistSpan, setPlaylistSpan) <- useState (32 :: Int)
  (selectedId, setSelectedId) <- useState (Nothing :: Maybe Int)
  (editorTab, setEditorTab) <- useState TabRhythm
  (compactPane, setCompactPane) <- useState PaneEditor
  (workspace, setWorkspace) <- useState (envStartView env)
  (arrangements, setArrangements) <- useState (envLayouts env)
  let arrangement = Map.findWithDefault (startingLayout workspace) workspace arrangements
      showWorkspace w = when (w /= workspace) (setWorkspace w)
  forM_ (zip ['1' ..] [minBound .. maxBound]) $ \(k, w) -> whenM (shortcut (ctrl <> key k)) (showWorkspace w)
  let compiled = dCompiled derived
      tracks = sequenceTracks song currentSeq
      selected = case selectedId of
        Just i | any ((== i) . trackId) tracks -> Just i
        _ -> trackId <$> listToMaybe tracks
      selectedIndex = selected >>= \i -> findIndex ((== i) . trackId) tracks
      selectTrack i = do
        when (Just i /= selectedId) (setSelectedId (Just i))
        when (ww < 1100) (setCompactPane PaneEditor)
      len = songLength song
      -- Where the transport is within the song, which loops at its end.
      songPosition
        | len > 0 = now - fromIntegral len * fromIntegral (floor (now / fromIntegral len) :: Int)
        | otherwise = now
      setPlayMode m = when (m /= mode) $ do
        setMode m
        liftIO (seekTo eng 0)
        requestFrame
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
      -- Project files.
      songFilter = FileFilter "LunarCycles song" (T.pack projectExtension)
      dirty = song /= projSaved project
      replaceSong s path = liftIO (writeIORef replacement (Just (s, path)))
      startDialog purpose = \case
        Just did -> setDialog (Just (purpose, did))
        Nothing -> setStatus "File dialogs need the desktop window"
      saveTo path = do
        r <- liftIO (saveSong path song)
        case r of
          Left e -> setStatus e
          Right written -> do
            setProject (Project (Just written) song)
            setStatus ("Saved " <> T.pack written)
      saveAs = startDialog ForSave =<< askSaveFileDialog defaultFileDialogOptions {dialogFilters = [songFilter], dialogDefaultLocation = projPath project}
      exportCycles = if mode == PlaySong && len > 0 then len else 16
      exportTo path = do
        let target = if null (takeExtension path) then path <.> "mid" else path
        r <- liftIO (try @SomeException (exportMidi target (songCps song) exportCycles (dAudible derived)))
        setStatus (either (\e -> "Export failed: " <> tshow e) (const ("Wrote " <> T.pack target)) r)
      projectOps =
        ProjectOps
          { opDirty = dirty
          , opName = maybe "this song" projectName (projPath project)
          , opNew = replaceSong blankSong Nothing >> setStatus "New song"
          , opOpen = startDialog ForOpen =<< askOpenFileDialog defaultFileDialogOptions {dialogFilters = [songFilter]}
          , opSave = maybe saveAs saveTo (projPath project)
          , opSaveAs = saveAs
          , opExport = startDialog ForExport =<< askSaveFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "MIDI file" "mid"]}
          , opExportNote =
              if mode == PlaySong && len > 0
                then "The whole song, " <> tshow len <> " cycles, rendered by Euterpea"
                else "16 cycles of " <> seqName currentSeq <> ", rendered by Euterpea"
          }
      undo = liftIO (writeIORef stepRequest (Just StepUndo))
      redo = liftIO (writeIORef stepRequest (Just StepRedo))
  forM_ dialog $ \(purpose, did) ->
    pollFileDialogUi did >>= \case
      FileDialogPending -> wakeAfter 0.1
      FileDialogSelected (path : _) -> do
        setDialog Nothing
        case purpose of
          ForOpen ->
            liftIO (loadSong path) >>= \case
              Left e -> setStatus e
              Right s -> replaceSong s (Just path) >> setStatus ("Opened " <> T.pack path)
          ForSave -> saveTo path
          ForExport -> exportTo path
      _ -> setDialog Nothing
  whenM (shortcut (ctrl <> key 'z')) undo
  whenM (shortcut (ctrl <> shift <> key 'z')) redo
  whenM (shortcut (ctrl <> key 'y')) redo
  setWindowTitleUi (maybe "Untitled" projectName (projPath project) <> (if dirty then " *" else "") <> " - LunarCycles")
  let docked = ww >= 1100
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
          PanePlaylist ->
            frame
              pctx
              (paneTitle kind)
              ( Just . (390,) $ do
                  let spans = [8, 16, 32, 64]
                  i <- selectField 124 (map (\n -> tshow n <> " cycles") spans) (fromMaybe 2 (elemIndex playlistSpan spans))
                  when (spans !! i /= playlistSpan) (setPlaylistSpan (spans !! i))
                  addResp <- buttonWith' (fontSize 13 . alignMid . minH 36) "+ Sequence"
                  tooltip addResp "Start an empty sequence"
                  when (respClicked addResp) $ do
                    let sid = nextSequenceId song
                    edit (\s -> s {songSequences = songSequences s <> [newSequence sid ("sequence " <> tshow sid)]})
                    pickSequence sid
              )
              $ columnWith (tight . gap 8 . fillW . fillH) $ do
                rowWith (tight . gap 10 . fillW . fillH) $ do
                  sequencePicker current song pickSequence edit
                  res <-
                    playlistGrid
                      current
                      sequences
                      (\sid -> Map.findWithDefault [] sid (dPreviews derived))
                      playlistSpan
                      (if mode == PlaySong then Just (Playhead songPosition playing) else Nothing)
                      (songPlaylist song)
                  when (prClips res /= songPlaylist song) $ edit (\s -> s {songPlaylist = prClips res})
                  forM_ (prPick res) pickSequence
                  forM_ (prOpen res) $ \sid -> pickSequence sid >> showWorkspace WsPattern
                  forM_ (prSeek res) $ \c -> do
                    when (mode /= PlaySong) (setMode PlaySong)
                    liftIO (seekTo eng c)
                    requestFrame
                hint "Click a lane to place the chosen sequence. Drag a clip to move it, or its right edge to stretch it. Right-click it, or point at it and press Delete, to remove it. Click the ruler to play from there."
          PaneTracks ->
            frame
              pctx
              (paneTitle kind)
              ( Just . (400,) $ do
                  let at = fromMaybe 0 (findIndex ((== current) . seqId) sequences)
                  i <- selectField 150 (map seqName sequences) at
                  when (i /= at) (pickSequence (seqId (sequences !! i)))
                  addResp <- buttonWith' (fontSize 13 . alignMid . minH 36) "+ Track"
                  tooltip addResp "Add a channel to every sequence, with a part in this one"
                  when (respClicked addResp) $ do
                    let tid = nextChannelId song
                    edit (addChannel current (newChannel tid ("track " <> tshow tid) "cp") (Part (Steps (replicate 8 Nothing)) []))
                    selectTrack tid
              )
              $ trackList th (rectW (pgcRect pctx)) selected selectTrack silenced (partsIn currentSeq) editTrack compiled
          PaneEditor ->
            frame pctx (paneTitle kind) Nothing $
              case [(i, c) | (i, c) <- zip [0 ..] compiled, Just (trackId (cTrack c)) == selected] of
                (i, c) : _ -> do
                  let tid = trackId (cTrack c)
                      duplicate = do
                        edit (fst . duplicateTrack current tid)
                        selectTrack (nextChannelId song)
                      remove = edit (removeChannel tid)
                      placement
                        | partsIn currentSeq tid = Nothing
                        | otherwise = Just ("Silent in " <> seqName currentSeq <> ". Give it a rhythm to add it here.")
                  trackEditor th i now editorTab setEditorTab (silenced (cTrack c)) placement (editTrack tid) duplicate remove c
                [] -> muted ("Add a track to " <> seqName currentSeq <> " to edit it here.")
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
    resetLayout <-
      styled flatSurface $
        toolbarView
          env
          Toolbar
            { tbSong = song
            , tbEdit = edit
            , tbPlaying = playing
            , tbNow = now
            , tbMode = mode
            , tbSetMode = setPlayMode
            , tbPosition = songPosition
            , tbUndo = if null (hPast history) then Nothing else Just undo
            , tbRedo = if null (hFuture history) then Nothing else Just redo
            , tbProject = projectOps
            }
    -- A wide window shows a view's panes together; a narrow one shows one
    -- pane at a time, and leaves the views' arrangements as they were.
    if docked
      then rowWith (padLRTB 20 20 0 4 . tight . fillW . gap 16 . alignMid) $ do
        next <- tabBarConfigured defaultTabsConfig {tabsStyle = TabUnderline} workspace
          [tab w (workspaceTitle w) () | w <- [minBound .. maxBound]]
        showWorkspace next
        flex
        labelWith (fontMuted . fontSize 12 . alignMid) "Ctrl+1, 2, 3 switch views"
      else rowWith (padXY 0 4 . tight . fillW) $ do
        next <- tabBarConfigured defaultTabsConfig {tabsStyle = TabUnderline} compactPane
          [tab k (paneTitle k) () | k <- [PaneRing, PaneTimeline, PaneTracks, PaneEditor, PaneCode, PanePlaylist]]
        when (next /= compactPane) (setCompactPane next)
    -- Each view keeps a grid of its own, and with it its maximized pane.
    gridResp <- withKey (if docked then fromEnum workspace else -1) $
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
      then do
        liftIO (forgetLayout (workspaceName workspace))
        setArrangements (Map.delete workspace arrangements)
      else when docked $ forM_ (pgrTree gridResp) $ \arranged -> do
        when (arranged /= arrangement) (setArrangements (Map.insert workspace arranged arrangements))
        when (pgrCommitted gridResp) (liftIO (saveLayout (workspaceName workspace) arranged))
    rowWith (padXY 20 8 . tight . fillW . gap 16 . alignMid) $ do
      labelWith (fontMuted . fontSize 12) (if playing then "Playing" else "Stopped")
      labelWith (fontMuted . fontSize 12) $ case mode of
        PlaySequence -> "Looping " <> seqName currentSeq
        PlaySong -> "Song, " <> tshow len <> (if len == 1 then " cycle" else " cycles")
      labelWith (fontMuted . fontSize 12) (tshow (length (seqParts currentSeq)) <> " of " <> tshow (length tracks) <> " tracks in " <> seqName currentSeq)
      -- What the last command or the engine had to say, on one line.
      msg <- liftIO (engineMessage eng)
      let note = T.intercalate ". " (filter (not . T.null) [status, msg])
      scope $ unless (T.null note) $ do
        fm <- uiFontMetrics
        shown <- truncateTextUi fm (max 80 (ww - 760)) note
        r <- labelWith' (fontMuted . fontSize 12) shown
        when (shown /= note) (tooltip r note)
      flex
      when docked $ labelWith (fontMuted . fontSize 12) "Space: play / stop    Ctrl+Z: undo"
  -- The frame's edits, or an undo, redo or new song, become the next song.
  f <- liftIO (readIORef pending)
  step <- liftIO (readIORef stepRequest)
  replaced <- liftIO (readIORef replacement)
  held <- mouseHeld MouseLeft
  inp <- askInput
  t <- uiTime
  let song' = f song
      keep s = when (s /= song) (setSong s) >> pure s
  case (replaced, step) of
    (Just (s, path), _) -> do
      setHistory emptyHistory
      setProject (Project path s)
      setCurrent Nothing
      keep s
    (_, Just StepUndo) | p : ps <- hPast history -> do
      setHistory history {hPast = ps, hFuture = song : hFuture history, hGroup = GroupNone}
      keep p
    (_, Just StepRedo) | n : ns <- hFuture history -> do
      setHistory history {hPast = song : hPast history, hFuture = ns, hGroup = GroupNone}
      keep n
    _
      | song' /= song -> do
          let group
                | held = GroupDrag
                | releasedIn MouseLeft inp || releasedIn MouseRight inp = GroupNone
                | otherwise = GroupTyping
              joins = group /= GroupNone && group == hGroup history && (group == GroupDrag || t - hAt history < 1)
          setHistory
            History
              { hPast = if joins then hPast history else take 200 (song : hPast history)
              , hFuture = []
              , hGroup = group
              , hAt = t
              }
          keep song'
      | otherwise -> do
          when (not held && hGroup history == GroupDrag) (setHistory history {hGroup = GroupNone})
          pure song

-- | A pane's frame: a title bar to drag it by, the pane's own controls and
-- a maximize button, over its body. The controls come with the pane width
-- they need beside the title and maximize button.
paneFrame :: Bool -> PaneGridCtx -> Text -> Maybe (Float, NanoUI ()) -> NanoUI () -> NanoUI PaneView
paneFrame docked pctx title controls body = do
  let Rect _ _ w h = pgcRect pctx
      titleBar = if docked then paneDragHandle pctx else rowWith
      -- Held to the split's rect, so a pane with more content than room
      -- scrolls instead of moving the dividers.
      held = if w > 0 && h > 0 then maxW w . maxH h else id
  panelWith (grow . held . padXY 16 12 . gap 12) $ do
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

-- | What the song file commands do, for the Project menu and its chords.
data ProjectOps = ProjectOps
  { opDirty :: Bool
  , opName :: Text
  , opNew :: NanoUI ()
  , opOpen :: NanoUI ()
  , opSave :: NanoUI ()
  , opSaveAs :: NanoUI ()
  , opExport :: NanoUI ()
  , opExportNote :: Text
  }

data Toolbar = Toolbar
  { tbSong :: Song
  , tbEdit :: Edit
  , tbPlaying :: Bool
  , tbNow :: Double
  , tbMode :: PlayMode
  , tbSetMode :: PlayMode -> NanoUI ()
  , tbPosition :: Double
    -- ^ Where the transport is within the song.
  , tbUndo :: Maybe (NanoUI ())
  , tbRedo :: Maybe (NanoUI ())
  , tbProject :: ProjectOps
  }

-- | Which command waits for the user to let unsaved changes go.
data Discard = DiscardForNew | DiscardForOpen
  deriving (Eq)

-- | The Project menu, transport, tempo, undo and redo, and a menu for where
-- the sound goes. True when Reset layout was pressed.
toolbarView :: AppEnv -> Toolbar -> NanoUI Bool
toolbarView env tb = do
  let eng = envEngine env
      song = tbSong tb
      playing = tbPlaying tb
      ops = tbProject tb
  (outputOpen, setOutputOpen) <- useFlag False
  (projectOpen, setProjectOpen) <- useFlag False
  (discard, setDiscard) <- useState (Nothing :: Maybe Discard)
  outs <- liftIO (midiOutputs eng)
  sel <- liftIO (selectedMidi eng)
  dirt <- liftIO (dirtStatus eng)
  samples <- liftIO (samplesInfo eng)
  (folderDialog, setFolderDialog) <- useState (Nothing :: Maybe FileDialogId)
  forM_ folderDialog $ \did ->
    pollFileDialogUi did >>= \case
      FileDialogPending -> wakeAfter 0.1
      FileDialogSelected (dir : _) -> setFolderDialog Nothing >> liftIO (setSampleFolder eng dir)
      _ -> setFolderDialog Nothing
  ww <- windowWidth
  -- New and Open ask first when they would lose unsaved changes.
  let unlessDirty d act
        | opDirty ops = setDiscard (Just d) >> setProjectOpen True
        | otherwise = act
      newSong = unlessDirty DiscardForNew (opNew ops)
      openSong = unlessDirty DiscardForOpen (opOpen ops)
  whenM (shortcut (ctrl <> key 'n')) newSong
  whenM (shortcut (ctrl <> key 'o')) openSong
  whenM (shortcut (ctrl <> key 's')) (opSave ops)
  whenM (shortcut (ctrl <> shift <> key 's')) (opSaveAs ops)
  panelWith (fillW . padXY 20 12 . tight) $ rowWith (tight . gap 16 . fillW . alignMid . wrap . lineGap 10) $ do
    labelWith (fontSize 20 . fontMedium . alignMid) "LunarCycles"
    -- The song file and its history, as a menu bar would hold them.
    projectResp <- buttonWith' (fontSize 14 . padXY 14 10 . minH 40 . alignMid) "Project  ▾"
    when (respClicked projectResp) (setProjectOpen (not projectOpen) >> setDiscard Nothing)
    (projectDismiss, _) <-
      popupWith projectOpen ((defaultPopupConfig (AnchorRect (respRect projectResp))) {cfgPlacement = PlacementBelow, cfgOffset = 8}) (fixedW (min 320 (ww - 24))) $
        case discard of
          Just d -> columnWith (padAll 12 . tight . gap 12 . fillW) $ do
            wrappedText id ("Discard the unsaved changes to " <> opName ops <> "?")
            rowWith (tight . gap 8 . fillW) $ do
              gone <- styled destructive (button "Discard")
              flex
              cancel <- button "Cancel"
              when (gone || cancel) (setDiscard Nothing >> setProjectOpen False)
              when gone $ case d of
                DiscardForNew -> opNew ops
                DiscardForOpen -> opOpen ops
          Nothing -> columnWith (padAll 6 . tight . gap 0 . fillW) $ do
            let item name chord act = whenM (menuItemShortcut name chord) (setProjectOpen False >> act)
            item "New" (ctrl <> key 'n') newSong
            item "Open…" (ctrl <> key 'o') openSong
            item "Save" (ctrl <> key 's') (opSave ops)
            item "Save as…" (ctrl <> shift <> key 's') (opSaveAs ops)
            menuSeparator
            whenM (menuItem "Export MIDI…") (setProjectOpen False >> opExport ops)
            columnWith (padXY 10 4 . tight . fillW) $ wrappedText (fontMuted . fontSize 12) (opExportNote ops)
    when (respClicked projectDismiss) (setProjectOpen False >> setDiscard Nothing)
    rowWith (tight . gap 6 . alignMid) $ do
      let historyButton glyph hintText action = do
            r <- disabledWhen (isNothing action) (buttonWith' (fixedWH 40 40 . alignMid) glyph)
            tooltip r hintText
            when (respClicked r) (sequence_ action)
      historyButton "↶" "Undo (Ctrl+Z)" (tbUndo tb)
      historyButton "↷" "Redo (Ctrl+Shift+Z)" (tbRedo tb)
    playResp <- styled primary $ buttonWith' (fixedWH 96 40 . alignMid) (if playing then "■ Stop" else "▶ Play")
    tooltip playResp "Space"
    when (respClicked playResp) $ do
      liftIO (setPlaying eng (not playing))
      requestFrame
      wakeAfter 0
    mode' <-
      tabBarConfigured
        defaultTabsConfig {tabsStyle = TabSegmented}
        (tbMode tb)
        [tab PlaySequence "Sequence" (), tab PlaySong "Song" ()]
    when (mode' /= tbMode tb) (tbSetMode tb mode')
    rowWith (tight . gap 6 . alignMid) $ do
      labelWith (fontMuted . fontSize 12 . alignMid) "Tempo"
      bpm <-
        numericInputConfigured
          defaultNumericInputConfig {nicMin = 30, nicMax = 300, nicDecimals = 1, nicStep = 1, nicLayout = alignMid (fixedW 88 defaultLayout)}
          (songCps song * 240)
      when (abs (bpm / 240 - songCps song) > 1e-6) $ tbEdit tb (\s -> s {songCps = bpm / 240})
    let len = songLength song
        twoPlaces v = showNum (fromIntegral (floor (v * 100) :: Int) / 100)
        readout = case tbMode tb of
          PlaySong | len > 0 -> "Cycle " <> twoPlaces (tbPosition tb) <> " / " <> tshow len
          _ -> "Cycle " <> twoPlaces (tbNow tb)
    when (ww >= 1100) $ labelWith (fontSize 12 . fixedW 132 . fontMuted . alignMid) readout
    flex
    let dirtOn = dirt `elem` [DirtOn, DirtStarting]
        outputName = case ["Samples" | siPlaying samples] <> maybe [] pure sel <> ["SuperDirt" | dirtOn] of
          [] -> "None"
          names -> T.intercalate " + " names
        menuWidth = min 420 (ww - 24)
    fm <- uiFontMetrics
    outLabel <- truncateTextUi fm (min 220 (ww - 168)) outputName
    outResp <- buttonWith' (fontSize 14 . padXY 14 10 . minH 40 . alignMid) ("Output: " <> outLabel <> "  ▾")
    when (respClicked outResp) (setOutputOpen (not outputOpen))
    (dismiss, _) <-
      popupWith outputOpen ((defaultPopupConfig (AnchorRect (respRect outResp))) {cfgPlacement = PlacementBelow, cfgOffset = 8}) (fixedW menuWidth) $
        columnWith (padAll 12 . tight . gap 12 . fillW) $ do
          wakeAfter 0.1
          labelWith fontMedium "Samples"
          rowWith (tight . gap 12 . fillW) $ do
            (sResp, on) <- toggleSwitchWith' alignMid (siOn samples)
            tooltip sResp "Play the patterns' samples in LunarCycles, from a folder laid out like Dirt-Samples"
            when (on /= siOn samples) (liftIO (setSamples eng on))
            wrappedText (fontMuted . fontSize 12 . alignMid) $ case (siError samples, siFolder samples) of
              (Just e, _) -> e
              (Nothing, Just dir) | siPlaying samples -> tshow (siSounds samples) <> " sounds in " <> T.pack dir
              _ -> "Off"
          chooseResp <- buttonWith' (minH 36 . padXY 12 8) "Choose folder…"
          tooltip chooseResp "A folder of sound folders (bd, sn, …) holding WAV files"
          when (respClicked chooseResp) $
            askOpenFolderDialog defaultFileDialogOptions {dialogDefaultLocation = siFolder samples} >>= \case
              Just did -> setFolderDialog (Just did)
              Nothing -> pure ()
          unless (null (siMissing samples) || not (siPlaying samples)) $
            hint ("No samples for " <> T.intercalate ", " (siMissing samples) <> ". Play these through MIDI or SuperDirt.")
          separator
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
    when (respClicked dismiss) (setOutputOpen False)
    reset <-
      if ww < 1100
        then pure False
        else do
          resetResp <- styled subtle (buttonWith' (fontSize 12 . alignMid . minH 36) "Reset layout")
          tooltip resetResp "Put this view's panes back where they started"
          pure (respClicked resetResp)
    pure reset

-- | Every channel on one line: its colour, name and pattern in the edited
-- sequence, and mute and solo. Clicking a line opens the track in the
-- editor. A channel without a part in the sequence shows a dash.
trackList :: Theme -> Float -> Maybe Int -> (Int -> NanoUI ()) -> (Track -> Bool) -> (Int -> Bool) -> (Int -> (Track -> Track) -> NanoUI ()) -> [Compiled] -> NanoUI ()
trackList th paneWidth selected selectTrack silenced placed editTrack compiled =
  scrollWith (fillW . fillH) $
    columnWith (padRight 4 . tight . gap 6 . fillW) $ do
      when (null compiled) $ hint "No tracks yet. Choose + Track to add a rhythm."
      forM_ (zip [0 ..] compiled) $ \(i, c) -> withKey (trackId (cTrack c)) $ do
        let t = cTrack c
            tid = trackId t
            col = trackColor th i
            isSel = selected == Just tid
            update = editTrack tid
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
                labelWith (fontMono . fontSize 12 . fontColor col . alignMid) ("d" <> tshow (i + 1))
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
                void $ richTextWith (fillW . padLeft 4 . fontMono . fontSize 12 . alignMid . fontColor (if silenced t then themeMuted th else styleFg (themePanel th))) [inlineText (if placed tid then cMini c else "—")]
        when (respClicked area) (selectTrack tid)

-- | The selected track: its name at the top, then tabs for its rhythm and
-- the functions it passes through in the edited sequence, and its sound,
-- which the channel has in every sequence. @placement@ says why the track
-- is silent here when the sequence gives it no part.
trackEditor :: Theme -> Int -> Double -> EditorTab -> (EditorTab -> NanoUI ()) -> Bool -> Maybe Text -> ((Track -> Track) -> NanoUI ()) -> NanoUI () -> NanoUI () -> Compiled -> NanoUI ()
trackEditor th index now editorTab setEditorTab silenced placement update duplicate remove c = do
  let t = cTrack c
      col = trackColor th index
      pitched = isPitched (trackSound t)
      beat = now - fromIntegral (floor now :: Int)
  (note, setNote) <- useText ""
  columnWith (tight . gap 8 . fillW . fillH) $ do
    rowWith (tight . gap 8 . fillW . alignMid) $ do
      labelWith (fontMono . fontColor col . fontSize 14 . alignMid) ("d" <> tshow (index + 1))
      name' <- textInputConfigured defaultTextInputConfig {ticLayout = alignMid (fillW (maxW 220 defaultLayout))} (trackName t)
      when (name' /= trackName t) $ update (\tr -> tr {trackName = name'})
      flex
      duplicateResp <- styled subtle (buttonWith' (fixedWH 36 36 . alignMid) "⧉")
      tooltip duplicateResp "Duplicate as a new channel, with this sequence's part"
      when (respClicked duplicateResp) duplicate
      removeResp <- styled subtle (buttonWith' (fixedWH 36 36 . alignMid) "✕")
      tooltip removeResp "Remove the channel from every sequence"
      when (respClicked removeResp) remove
    tab' <-
      tabBarConfigured
        defaultTabsConfig {tabsStyle = TabUnderline}
        editorTab
        [tab TabRhythm "Rhythm" (), tab TabFunctions "Functions" (), tab TabSound "Sound" ()]
    when (tab' /= editorTab) (setEditorTab tab')
    let expression = T.intercalate " $ " (map (T.concat . map tokText . transformCode . snd) (trackChain t) <> [sourceCode c])
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
          scope $ case (trackSource t, cRule c) of
            (Steps _, Just rule) -> wrappedText (fontMuted . fontSize 12 . alignMid) ("Simplified: " <> rule)
            _ -> pure ()
        scope $ unless (T.null note) $ wrappedText (fontTone Warning . fontSize 12) note
        scope $ forM_ placement $ wrappedText (fontMuted . fontSize 12)
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
        chain' <- transformChain col (sourceCode c) (trackChain t)
        when (chain' /= trackChain t) $ update (\tr -> tr {trackChain = chain'})
        hint "Functions read left to right as the code does: the first wraps all the others. Drag a block to reorder it, click it to change it."
      TabSound -> do
        rowWith (tight . gap 8 . alignMid) $ do
          labelWith (fontMuted . alignMid) "Sound"
          sound' <- soundField (trackSound t)
          forM_ sound' $ \s -> update (\tr -> tr {trackSound = s})
        knobRow col now t update c
        hint "The sound and knobs belong to the channel, so every sequence plays them. Type to filter the sounds, or enter any SuperDirt sample name. Drag, scroll or use arrow keys to set a knob. Right-click to reset. Use the signal button to add modulation."

-- | A track's sound as a combo box: the catalog is a filterable list that
-- scrolls rather than running off the window, and any other sample name can
-- be typed. Returns the sound to switch to when a pick or typed name commits.
soundField :: Text -> NanoUI (Maybe Text)
soundField current = columnWith (fixedW 240 . tight) $ do
  -- Text typed but not yet committed; the field otherwise shows the track's
  -- sound, so undo and switching tracks update it.
  (draft, setDraft) <- useState (Nothing :: Maybe Text)
  (resp, text) <- comboBox' "Sound" (map soundLabel catalog) (fromMaybe current draft)
  if respChanged resp
    then do
      when (isJust draft) (setDraft Nothing)
      pure (mfilter (/= current) (parseSound text))
    else do
      let draft' = if text == current then Nothing else Just text
      when (draft' /= draft) (setDraft draft')
      pure Nothing
  where
    parseSound txt = case find ((== txt) . soundLabel) catalog of
      Just s -> Just (soundName s)
      Nothing -> case T.words txt of
        [w] | T.all (\ch -> isAlphaNum ch || ch == '_' || ch == '-') w -> Just w
        _ -> Nothing

-- | The source as it appears at the end of the code's chain.
sourceCode :: Compiled -> Text
sourceCode c
  | isPitched (trackSound t) = "note \"" <> cMini c <> "\" # s \"" <> trackSound t <> "\""
  | otherwise = "s \"" <> cMini c <> "\""
  where
    t = cTrack c

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
