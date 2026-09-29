-- | Playback: the transport clock the views draw from, a scheduler thread
-- that queries the patterns a little ahead of the clock and hands their
-- notes to the built-in sampler and Euterpea's MIDI output, and an optional
-- Tidal stream to SuperDirt.
module Lunar.Engine
  ( Engine
  , MidiOut (..)
  , DirtStatus (..)
  , newEngine
  , shutdownEngine
  , engineCycle
  , enginePlaying
  , setPlaying
  , seekTo
  , setCps
  , setTracks
  , midiOutputs
  , selectedMidi
  , selectMidi
  , rescanMidi
  , dirtStatus
  , setDirt
  , soundFont
  , fluidRunning
  , startFluid
  , SamplesInfo (..)
  , samplesInfo
  , setSamples
  , setSampleFolder
  , setProjectSampleFolder
  , engineMessage
  ) where

import Control.Concurrent
import Control.Exception (SomeException, bracket, try)
import Control.Monad
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (find, isPrefixOf, isSuffixOf, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Euterpea.IO.MIDI.MidiIO
  ( DeviceInfo (..)
  , Message (..)
  , MidiMessage (..)
  , OutputDeviceID
  , deliverMidiEvent
  , getAllDevices
  , initializeMidi
  , outputMidi
  , terminateMidi
  )
import GHC.Clock (getMonotonicTime)
import Lunar.Catalog (isPitched)
import Lunar.Compile
import Lunar.Midi
import Lunar.Model (trackSound)
import Lunar.Sampler
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import Sound.Tidal.ID (ID (..))
import Sound.Tidal.Pattern (EventF (..), eventHasOnset, silence, wholeStart)
import Sound.Tidal.Stream qualified as Tidal
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, getHomeDirectory, getTemporaryDirectory, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hClose, hPutStr, openTempFile)
import System.IO.Error (isAlreadyInUseError)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess, terminateProcess)
import System.Timeout (timeout)

data Transport = Transport
  { tPlaying :: !Bool
  , tCps :: !Double
  , tAnchorTime :: !Double
  , tAnchorCycle :: !Double
  , tStart :: !Double
    -- ^ Where Play starts from, and Stop rewinds to.
  }

-- | Where the transport is at a monotonic time, in cycles.
cycleAt :: Transport -> Double -> Double
cycleAt t now
  | tPlaying t = tAnchorCycle t + (now - tAnchorTime t) * tCps t
  | otherwise = tAnchorCycle t

data MidiOut = MidiOut {moId :: !OutputDeviceID, moName :: !Text}

data MidiState = MidiState
  { msOutputs :: ![MidiOut]
  , msSelected :: !(Maybe MidiOut)
  , msPrograms :: !(Map.Map Int Int)
    -- ^ The program each channel was last set to, so a change is sent once.
  }

data SamplerState = SamplerState
  { ssPrefs :: !SamplePrefs
  , ssFolder :: !(Maybe FilePath)
    -- ^ The folder chosen, or else where SuperCollider keeps Dirt-Samples.
  , ssSampler :: !(Maybe Sampler)
  , ssError :: !(Maybe Text)
  }

data DirtStatus = DirtOff | DirtStarting | DirtOn | DirtFailed !Text
  deriving (Eq, Show)

data DirtMode = DirtAuto | DirtAll | DirtDisabled
  deriving (Eq)

data DirtServer = DirtServer
  { dsProcess :: !(Maybe ProcessHandle)
  , dsOwnsServer :: !Bool
  }

data Engine = Engine
  { engTransport :: !(IORef Transport)
  , engTracks :: !(IORef [Compiled])
  , engCursor :: !(IORef Double)
    -- ^ The cycle up to which notes have been sent.
  , engMidi :: !(MVar MidiState)
  , engSampler :: !(MVar SamplerState)
  , engDirt :: !(IORef (Maybe Tidal.Stream))
  , engDirtStatus :: !(IORef DirtStatus)
  , engDirtIds :: !(IORef [String])
  , engDirtMode :: !(IORef DirtMode)
  , engDirtServer :: !(IORef (Maybe DirtServer))
  , engDirtStartLock :: !(MVar ())
  , engShuttingDown :: !(IORef Bool)
  , engFluid :: !(IORef (Maybe ProcessHandle))
  , engMidiExplicit :: !(IORef Bool)
  , engAutoStart :: !(IORef Bool)
  , engMessage :: !(IORef Text)
  , engThreads :: !(IORef [ThreadId])
  }

-- | How far ahead of the clock notes are handed to MIDI, in seconds.
lookahead :: Double
lookahead = 0.1

newEngine :: Double -> IO Engine
newEngine cps = do
  now <- getMonotonicTime
  autoStart <- maybe True (/= "1") <$> lookupEnv "LUNAR_CYCLES_DISABLE_AUDIO_AUTOSTART"
  midiOk <- try initializeMidi
  outs <- either (\(_ :: SomeException) -> pure []) (const listOutputs) midiOk
  prefs <- loadSamplePrefs
  folder <- maybe defaultSampleFolder (pure . Just) (spFolder prefs)
  let samples = SamplerState prefs folder Nothing Nothing
  sampler <- if spOn prefs then openIn samples else pure samples
  eng <-
    Engine
      <$> newIORef (Transport False cps now 0 0)
      <*> newIORef []
      <*> newIORef 0
      <*> newMVar (MidiState outs (defaultOutput outs) Map.empty)
      <*> newMVar sampler
      <*> newIORef Nothing
      <*> newIORef DirtOff
      <*> newIORef []
      <*> newIORef DirtAuto
      <*> newIORef Nothing
      <*> newMVar ()
      <*> newIORef False
      <*> newIORef Nothing
      <*> newIORef False
      <*> newIORef autoStart
      <*> newIORef (either (const "MIDI unavailable: PortMidi did not start") (const "") midiOk)
      <*> newIORef []
  tid <- forkIO (schedulerLoop eng)
  writeIORef (engThreads eng) [tid]
  pure eng

-- | Prefer a synthesizer over PortMidi's loopback ports.
defaultOutput :: [MidiOut] -> Maybe MidiOut
defaultOutput outs =
  listToMaybe (filter (\o -> not ("Midi Through" `T.isInfixOf` moName o)) outs)

listOutputs :: IO [MidiOut]
listOutputs = do
  (_, outs) <- getAllDevices
  pure [MidiOut i (T.pack (name info)) | (i, info) <- outs]

shutdownEngine :: Engine -> IO ()
shutdownEngine eng = do
  writeIORef (engShuttingDown eng) True
  mapM_ killThread =<< readIORef (engThreads eng)
  withMVar (engDirtStartLock eng) (const (pure ()))
  readIORef (engDirt eng) >>= mapM_ Tidal.streamHush
  stopDirtServer eng
  withMVar (engMidi eng) $ \ms -> forM_ (msSelected ms) (allNotesOff . moId)
  _ <- try @SomeException terminateMidi
  mapM_ terminateProcess =<< readIORef (engFluid eng)
  withMVar (engSampler eng) (mapM_ closeSampler . ssSampler)

allNotesOff :: OutputDeviceID -> IO ()
allNotesOff dev = do
  forM_ [0 .. 15] $ \ch -> deliverMidiEvent dev (0, Std (ControlChange ch 123 0))
  outputMidi dev

engineCycle :: Engine -> IO Double
engineCycle eng = cycleAt <$> readIORef (engTransport eng) <*> getMonotonicTime

enginePlaying :: Engine -> IO Bool
enginePlaying eng = tPlaying <$> readIORef (engTransport eng)

-- | Start from the start point, or stop and rewind to it.
setPlaying :: Engine -> Bool -> IO ()
setPlaying eng on = do
  now <- getMonotonicTime
  t <- readIORef (engTransport eng)
  when (on /= tPlaying t) $ do
    writeIORef (engTransport eng) t {tPlaying = on, tAnchorTime = now, tAnchorCycle = tStart t}
    writeIORef (engCursor eng) (tStart t)
    unless on $ do
      withMVar (engMidi eng) $ \ms -> forM_ (msSelected ms) (allNotesOff . moId)
      withSampler eng cancelPending
    readIORef (engDirt eng) >>= mapM_ (\st -> when on (Tidal.streamSetCycle st (toRational (tStart t))))
    when on $ readIORef (engTracks eng) >>= autoStartOutputs eng
    pushDirt eng

-- | Move the start point to a cycle, and the transport with it: playback
-- carries on from there, and Stop comes back to it.
seekTo :: Engine -> Double -> IO ()
seekTo eng c = do
  now <- getMonotonicTime
  playing <- atomicModifyIORef' (engTransport eng) $ \t ->
    (t {tAnchorTime = now, tAnchorCycle = c, tStart = c}, tPlaying t)
  writeIORef (engCursor eng) c
  withSampler eng cancelPending
  when playing $ readIORef (engDirt eng) >>= mapM_ (\st -> Tidal.streamSetCycle st (toRational c))

-- | Change tempo without a jump: the clock is re-anchored where it is now.
setCps :: Engine -> Double -> IO ()
setCps eng cps = do
  now <- getMonotonicTime
  atomicModifyIORef' (engTransport eng) $ \t ->
    (t {tCps = cps, tAnchorTime = now, tAnchorCycle = cycleAt t now}, ())
  readIORef (engDirt eng) >>= mapM_ (\st -> Tidal.streamSetCPS st (toRational cps))

-- | The tracks to play, already filtered for mute and solo.
setTracks :: Engine -> [Compiled] -> IO ()
setTracks eng comps = do
  writeIORef (engTracks eng) comps
  pushDirt eng
  playing <- enginePlaying eng
  when playing (autoStartOutputs eng comps)

-- | Every few milliseconds, hand over the notes that fall due. A failure,
-- such as a pattern that throws when queried, is reported rather than left
-- to end the loop.
schedulerLoop :: Engine -> IO ()
schedulerLoop eng = forever $ do
  threadDelay 4000
  r <- try @SomeException (scheduleDue eng)
  either (\e -> writeIORef (engMessage eng) ("Playback: " <> T.pack (show e))) pure r

scheduleDue :: Engine -> IO ()
scheduleDue eng = do
  now <- getMonotonicTime
  t <- readIORef (engTransport eng)
  due <-
    if not (tPlaying t)
      then pure []
      else do
        let cNow = cycleAt t now
            horizon = cNow + lookahead * tCps t
        from <- atomicModifyIORef' (engCursor eng) $ \c ->
          (horizon, if c < cNow - 0.25 || c > horizon then cNow else c)
        comps <- if horizon > from then readIORef (engTracks eng) else pure []
        pure
          [ (c, e, (on - cNow) / tCps t)
          | c <- comps
          , e <- eventsIn from horizon (cPattern c)
          , eventHasOnset e
          , let on = fromRational (wholeStart e)
          , on >= from && on < horizon
          ]
  mode <- readIORef (engDirtMode eng)
  dirtOn <- (== DirtOn) <$> readIORef (engDirtStatus eng)
  midiDue <-
    if mode == DirtAuto && dirtOn
      then pure (filter (not . trackNeedsDirt . dueTrack) due)
      else pure due
  let notes = sortOn fst [(delay, n) | (c, e, delay) <- midiDue, Just n <- [eventNote (tCps t) (cChannel c) e]]
  unless (null notes) $ modifyMVar_ (engMidi eng) $ \ms ->
    case msSelected ms of
      Nothing -> pure ms
      Just out -> foldM (sendNote (moId out)) ms notes
  withMVar (engMidi eng) $ \ms -> forM_ (msSelected ms) (outputMidi . moId)
  -- After MIDI is out, since a sound's first note loads its file.
  unless (null due) $ withSampler eng $ \smp ->
    forM_ due $ \(_, e, delay) -> playEvent smp (now + delay) (value e)
  where
    dueTrack (comp, _, _) = comp

sendNote :: OutputDeviceID -> MidiState -> (Double, MidiNote) -> IO MidiState
sendNote dev ms (delay, n) = do
  let at = max 0.0005 delay
      ch = mnChannel n
      -- Euterpea's queue does not keep the order of messages due at the same
      -- moment, and an NRPN only reads in order, so each message is a
      -- microsecond behind the one before.
      step i = at + fromIntegral (i :: Int) * 1e-6
  programs <- case mnProgram n of
    Just p | Map.lookup ch (msPrograms ms) /= Just p -> do
      deliverMidiEvent dev (at, Std (ProgramChange ch p))
      pure (Map.insert ch p (msPrograms ms))
    _ -> pure (msPrograms ms)
  forM_ (zip [1 ..] (mnControls n)) $ \(i, (cc, v)) -> deliverMidiEvent dev (step i, Std (ControlChange ch cc v))
  deliverMidiEvent dev (step (length (mnControls n) + 1), ANote ch (mnKey n) (mnVelocity n) (mnSeconds n))
  pure ms {msPrograms = programs}

midiOutputs :: Engine -> IO [MidiOut]
midiOutputs eng = msOutputs <$> readMVar (engMidi eng)

selectedMidi :: Engine -> IO (Maybe Text)
selectedMidi eng = fmap moName . msSelected <$> readMVar (engMidi eng)

selectMidi :: Engine -> Maybe Text -> IO ()
selectMidi eng pick = do
  when (isJust pick) $ do
    mode <- readIORef (engDirtMode eng)
    when (mode == DirtAuto) (silenceAutoDirt eng DirtAuto)
  writeIORef (engMidiExplicit eng) True
  modifyMVar_ (engMidi eng) $ \ms -> do
    forM_ (msSelected ms) (allNotesOff . moId)
    pure ms {msSelected = pick >>= \nm -> find ((== nm) . moName) (msOutputs ms), msPrograms = Map.empty}

preferManualMidi :: Engine -> IO ()
preferManualMidi eng = do
  mode <- readIORef (engDirtMode eng)
  when (mode == DirtAuto) (silenceAutoDirt eng DirtDisabled)

silenceAutoDirt :: Engine -> DirtMode -> IO ()
silenceAutoDirt eng mode = do
  writeIORef (engDirtMode eng) mode
  readIORef (engDirt eng) >>= mapM_ Tidal.streamHush
  writeIORef (engDirtIds eng) []
  writeIORef (engDirtStatus eng) DirtOff
  writeIORef (engMessage eng) ""

-- | Restart PortMidi so devices that appeared since start are listed. The
-- chosen device is kept by name when it is still there.
rescanMidi :: Engine -> IO ()
rescanMidi eng = do
  shuttingDown <- readIORef (engShuttingDown eng)
  unless shuttingDown $ modifyMVar_ (engMidi eng) $ \ms -> do
    r <- try @SomeException $ do
      terminateMidi
      initializeMidi
      listOutputs
    case r of
      Left e -> do
        writeIORef (engMessage eng) ("MIDI rescan failed: " <> T.pack (show e))
        pure ms {msOutputs = [], msSelected = Nothing}
      Right outs -> do
        let kept = msSelected ms >>= \sel -> find ((== moName sel) . moName) outs
        writeIORef (engMessage eng) (T.pack (show (length outs)) <> " MIDI outputs")
        explicit <- readIORef (engMidiExplicit eng)
        pure ms {msOutputs = outs, msSelected = maybe (if explicit then Nothing else defaultOutput outs) Just kept, msPrograms = Map.empty}

dirtStatus :: Engine -> IO DirtStatus
dirtStatus eng = readIORef (engDirtStatus eng)

-- | Start or stop sending to SuperDirt on 127.0.0.1:57120.
setDirt :: Engine -> Bool -> IO ()
setDirt eng on = do
  writeIORef (engDirtMode eng) (if on then DirtAll else DirtDisabled)
  if on
    then requestDirt eng
    else do
      readIORef (engDirt eng) >>= mapM_ Tidal.streamHush
      writeIORef (engDirtIds eng) []
      writeIORef (engDirtStatus eng) DirtOff
      writeIORef (engMessage eng) ""

requestDirt :: Engine -> IO ()
requestDirt eng = do
  start <- atomicModifyIORef' (engDirtStatus eng) $ \case
    DirtOn -> (DirtOn, False)
    DirtStarting -> (DirtStarting, False)
    _ -> (DirtStarting, True)
  if start
    then do
      writeIORef (engMessage eng) "Starting SuperDirt…"
      void (forkIO (startDirtStream eng))
    else pushDirt eng

startDirtStream :: Engine -> IO ()
startDirtStream eng = withMVar (engDirtStartLock eng) $ \_ -> do
  stopping <- readIORef (engShuttingDown eng)
  unless stopping $ do
    attempt <- try @SomeException (startDirt eng)
    case either (Left . T.pack . show) id attempt of
      Left message -> failDirt eng message
      Right () -> finishDirtStart eng

startDirt :: Engine -> IO (Either Text ())
startDirt eng = do
  mode <- readIORef (engDirtMode eng)
  sampler <- readMVar (engSampler eng)
  ensureDirtServer eng (mode == DirtAll) (ssFolder sampler) >>= \case
    Left message -> pure (Left message)
    Right server -> do
      writeIORef (engDirtServer eng) (Just server)
      requested <- readIORef (engDirtStatus eng)
      shuttingDown <- readIORef (engShuttingDown eng)
      if requested /= DirtStarting || shuttingDown
        then pure (Right ())
        else do
          stream <- readIORef (engDirt eng) >>= \case
            Just st -> pure st
            Nothing -> Tidal.startTidal
              (Tidal.superdirtTarget {Tidal.oLatency = 0.1, Tidal.oAddress = "127.0.0.1", Tidal.oPort = 57120})
              Tidal.defaultConfig {Tidal.cVerbose = False, Tidal.cCtrlListen = False}
          writeIORef (engDirt eng) (Just stream)
          t <- readIORef (engTransport eng)
          Tidal.streamSetCPS stream (toRational (tCps t))
          now <- getMonotonicTime
          Tidal.streamSetCycle stream (toRational (cycleAt t now))
          pure (Right ())

finishDirtStart :: Engine -> IO ()
finishDirtStart eng = do
  started <- atomicModifyIORef' (engDirtStatus eng) $ \case
    DirtStarting -> (DirtOn, True)
    status -> (status, False)
  stopping <- readIORef (engShuttingDown eng)
  if started && not stopping
    then writeIORef (engMessage eng) "" >> pushDirt eng
    else readIORef (engDirt eng) >>= mapM_ Tidal.streamHush >> stopDirtServer eng

failDirt :: Engine -> Text -> IO ()
failDirt eng message = do
  failed <- atomicModifyIORef' (engDirtStatus eng) $ \case
    DirtStarting -> (DirtFailed message, True)
    status -> (status, False)
  when failed (writeIORef (engMessage eng) ("SuperDirt: " <> message))
  stopDirtServer eng
  when failed (autoFluidFallback eng)

-- | Hand SuperDirt the current patterns, or silence while stopped.
pushDirt :: Engine -> IO ()
pushDirt eng = do
  status <- readIORef (engDirtStatus eng)
  mst <- readIORef (engDirt eng)
  case (status, mst) of
    (DirtOn, Just st) -> do
      playing <- enginePlaying eng
      comps <- readIORef (engTracks eng)
      old <- readIORef (engDirtIds eng)
      mode <- readIORef (engDirtMode eng)
      let route comp = case mode of
            DirtAuto -> trackNeedsDirt comp
            DirtAll -> True
            DirtDisabled -> False
          current = [("lunar" <> show (cChannel c), cPattern c) | playing, c <- comps, route c]
      forM_ current $ \(i, p) -> Tidal.streamReplace st (ID i) p
      forM_ (filter (`notElem` map fst current) old) $ \i -> Tidal.streamReplace st (ID i) silence
      writeIORef (engDirtIds eng) (map fst current)
    _ -> pure ()

autoStartOutputs :: Engine -> [Compiled] -> IO ()
autoStartOutputs eng comps = do
  enabled <- readIORef (engAutoStart eng)
  mode <- readIORef (engDirtMode eng)
  explicitMidi <- readIORef (engMidiExplicit eng)
  selectedMidiOut <- msSelected <$> readMVar (engMidi eng)
  status <- readIORef (engDirtStatus eng)
  let midiChosen = explicitMidi && isJust selectedMidiOut
  when (enabled && mode == DirtAuto && status == DirtOff && any trackNeedsDirt comps && not midiChosen) $ requestDirt eng

trackNeedsDirt :: Compiled -> Bool
trackNeedsDirt = isPitched . trackSound . cTrack

ensureDirtServer :: Engine -> Bool -> Maybe FilePath -> IO (Either Text DirtServer)
ensureDirtServer eng loadSamples sampleFolder = readIORef (engDirtServer eng) >>= \case
  Just server -> serverRunning server >>= \case
    True -> pure (Right server)
    False -> start
  Nothing -> start
  where
    serverRunning (DirtServer (Just process) _) = maybe True (const False) <$> getProcessExitCode process
    serverRunning (DirtServer Nothing _) = either (const False) id <$> superDirtListening
    start = do
      writeIORef (engDirtServer eng) Nothing
      superDirtListening >>= \case
        Left e -> pure (Left ("Could not check the SuperDirt port: " <> e))
        Right True -> pure (Right (DirtServer Nothing False))
        Right False -> launchDirtServer eng loadSamples sampleFolder

superDirtListening :: IO (Either Text Bool)
superDirtListening = do
  listening <- dirtHandshake
  if listening then pure (Right True) else do
    portInUse 57120 >>= \case
      Right False -> pure (Right False)
      Right True -> pure (Left "UDP port 57120 is occupied, but no SuperDirt handshake replied.")
      Left e -> pure (Left e)

dirtHandshake :: IO Bool
dirtHandshake = do
  reply <- try @SomeException $ Socket.withSocketsDo $ bracket
    (Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol)
    Socket.close
    (\sock -> do
       Socket.connect sock (Socket.SockAddrInet 57120 (Socket.tupleToHostAddress (127, 0, 0, 1)))
       void (SocketBytes.send sock (oscMessage "/dirt/handshake"))
       timeout 100000 (SocketBytes.recv sock 2048)
    )
  pure $ case reply of
    Right (Just packet) -> BS.isPrefixOf (oscString "/dirt/handshake/reply") packet
    _ -> False

oscMessage :: String -> BS.ByteString
oscMessage address = oscString address <> oscString ","

oscString :: String -> BS.ByteString
oscString value = raw <> BS.replicate padding 0
  where
    raw = BS.pack (map (fromIntegral . fromEnum) value <> [0])
    padding = (4 - BS.length raw `mod` 4) `mod` 4

portInUse :: Int -> IO (Either Text Bool)
portInUse port = do
  result <- try @IOError $ Socket.withSocketsDo $ bracket
    (Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol)
    Socket.close
    (\sock -> Socket.bind sock (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1))))
  pure $ case result of
    Right () -> Right False
    Left e
      | isAlreadyInUseError e -> Right True
      | otherwise -> Left (T.pack (show e))

launchDirtServer :: Engine -> Bool -> Maybe FilePath -> IO (Either Text DirtServer)
launchDirtServer eng loadSamples sampleFolder = findExecutable "sclang" >>= \case
  Nothing -> pure (Left "SuperCollider (sclang) is not installed.")
  Just sclang -> do
    tmp <- getTemporaryDirectory
    (script, scriptHandle) <- openTempFile tmp "lunar-cycles-superdirt.scd"
    let stateFile = script <> ".state"
        cleanup = mapM_ removeQuietly [script, stateFile]
    hPutStr scriptHandle (superDirtScript stateFile loadSamples sampleFolder)
    hClose scriptHandle
    spawned <- try @SomeException (spawnProcess sclang ["-D", script])
    case spawned of
      Left e -> cleanup >> pure (Left ("Could not start sclang: " <> T.pack (show e)))
      Right process -> do
        ready <- waitForDirt eng process stateFile (if loadSamples then 600 else 150)
        case ready of
          Right ownsServer -> cleanup >> pure (Right (DirtServer (Just process) ownsServer))
          Left message -> do
            state <- readDirtState stateFile
            stopDirtProcess process (state == Just "owned")
            cleanup
            pure (Left message)

superDirtScript :: FilePath -> Bool -> Maybe FilePath -> String
superDirtScript stateFile loadSamples sampleFolder =
  "(\n"
    <> "var statePath = " <> scString stateFile <> ";\n"
    <> "var report = { |state| var f = File(statePath, \"w\"); f.write(state); f.close; };\n"
    <> "var startDirt = { |owned|\n"
    <> "    var dirt;\n"
    <> "    report.value(if(owned, \"owned\", \"attached\"));\n"
    <> "    dirt = \\SuperDirt.asClass.new(2, s);\n"
    <> "    ~lunarDirt = dirt;\n"
    <> loadSoundFiles
    <> "    s.sync;\n"
    <> "    dirt.start(57120, 0 ! 12);\n"
    <> "};\n"
    <> "var dirtClass = \\SuperDirt.asClass;\n"
    <> "if(dirtClass.isNil) { report.value(\"missing\"); 0.exit; } {\n"
    <> "    s = Server.local;\n"
    <> "    s.latency = 0.1;\n"
    <> "    s.startAliveThread;\n"
    <> "    Routine {\n"
    <> "        1.0.wait;\n"
    <> "        if(s.serverRunning) { startDirt.value(false); } {\n"
    <> "            s.waitForBoot { startDirt.value(true); };\n"
    <> "            s.boot;\n"
    <> "        };\n"
    <> "    }.play(SystemClock);\n"
    <> "};\n"
    <> ")\n"
  where
    loadSoundFiles
      | not loadSamples = ""
      | otherwise = case sampleFolder of
          Just folder -> "    dirt.loadSoundFiles(" <> scString (folder </> "*") <> ");\n"
          Nothing -> "    dirt.loadSoundFiles;\n"

scString :: FilePath -> String
scString path = '"' : concatMap escape path <> "\""
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape c = [c]

waitForDirt :: Engine -> ProcessHandle -> FilePath -> Int -> IO (Either Text Bool)
waitForDirt eng process stateFile attempts
  | attempts <= 0 = pure (Left "SuperCollider did not start SuperDirt before the startup timeout.")
  | otherwise = do
      stopping <- readIORef (engShuttingDown eng)
      requested <- readIORef (engDirtStatus eng)
      if stopping || requested /= DirtStarting
        then pure (Left "SuperDirt startup cancelled.")
        else do
          state <- readDirtState stateFile
          if state == Just "missing"
            then pure (Left "The SuperDirt Quark is not installed in SuperCollider.")
            else dirtHandshake >>= \case
              True -> pure (Right (state == Just "owned"))
              False -> getProcessExitCode process >>= \case
                Just status -> pure (Left ("sclang exited before SuperDirt was ready: " <> T.pack (show status)))
                Nothing -> threadDelay 100000 >> waitForDirt eng process stateFile (attempts - 1)

readDirtState :: FilePath -> IO (Maybe String)
readDirtState path = do
  exists <- doesFileExist path
  if exists
    then do
      result <- try @SomeException $ do
        contents <- readFile path
        length contents `seq` pure contents
      pure (either (const Nothing) Just result)
    else pure Nothing

removeQuietly :: FilePath -> IO ()
removeQuietly path = void (try @SomeException (removeFile path))

terminateIfRunning :: ProcessHandle -> IO ()
terminateIfRunning process = do
  status <- getProcessExitCode process
  when (isNothing status) (void (try @SomeException (terminateProcess process)))

stopDirtProcess :: ProcessHandle -> Bool -> IO ()
stopDirtProcess process ownsServer = do
  when ownsServer (sendServerQuit >> threadDelay 250000)
  terminateIfRunning process

sendServerQuit :: IO ()
sendServerQuit = void . try @SomeException $ Socket.withSocketsDo $ bracket
  (Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol)
  Socket.close
  (\sock -> do
     Socket.connect sock (Socket.SockAddrInet 57110 (Socket.tupleToHostAddress (127, 0, 0, 1)))
     SocketBytes.send sock (oscMessage "/quit")
  )

stopDirtServer :: Engine -> IO ()
stopDirtServer eng = do
  server <- atomicModifyIORef' (engDirtServer eng) (\s -> (Nothing, s))
  forM_ server $ \owned -> forM_ (dsProcess owned) $ \process ->
    stopDirtProcess process (dsOwnsServer owned)

autoFluidFallback :: Engine -> IO ()
autoFluidFallback eng = do
  enabled <- readIORef (engAutoStart eng)
  shuttingDown <- readIORef (engShuttingDown eng)
  explicit <- readIORef (engMidiExplicit eng)
  mode <- readIORef (engDirtMode eng)
  selected <- msSelected <$> readMVar (engMidi eng)
  when (enabled && not shuttingDown && mode /= DirtDisabled && not explicit && isNothing selected) $ soundFont >>= \case
    Just sf -> startFluidAutomatically eng sf
    Nothing -> writeIORef (engMessage eng) "No SuperDirt or MIDI output available, and no SoundFont was found."

-- | The first General MIDI soundfont in the usual places, for FluidSynth.
soundFont :: IO (Maybe FilePath)
soundFont = do
  home <- getHomeDirectory
  let dirs =
        [ "/usr/share/soundfonts"
        , "/usr/share/sounds/sf2"
        , "/usr/share/sounds/sf3"
        , "/usr/local/share/soundfonts"
        , home <> "/.local/share/soundfonts"
        ]
  found <- forM dirs $ \d -> do
    ok <- doesDirectoryExist d
    if not ok
      then pure []
      else map ((d <> "/") <>) . filter isFont <$> listDirectory d
  pure (listToMaybe (concat found))
  where
    isFont f = (".sf2" `isSuffixOf` f || ".sf3" `isSuffixOf` f) && not ("." `isPrefixOf` f)

fluidRunning :: Engine -> IO Bool
fluidRunning eng =
  readIORef (engFluid eng) >>= \case
    Nothing -> pure False
    Just h -> (== Nothing) <$> getProcessExitCode h

-- | Start FluidSynth as an ALSA sequencer client playing the soundfont, then
-- list MIDI outputs again once it has had time to register its port.
startFluid :: Engine -> FilePath -> IO ()
startFluid eng = startFluidWithSelection eng True

startFluidAutomatically :: Engine -> FilePath -> IO ()
startFluidAutomatically eng = startFluidWithSelection eng False

startFluidWithSelection :: Engine -> Bool -> FilePath -> IO ()
startFluidWithSelection eng explicitRequest sf = do
  when explicitRequest $ do
    writeIORef (engMidiExplicit eng) True
    preferManualMidi eng
  running <- fluidRunning eng
  if running
    then do
      rescanMidi eng
      selectFluidOutput eng explicitRequest
    else do
      r <- try @SomeException (spawnProcess "fluidsynth" ["-a", "pipewire", "-m", "alsa_seq", "-i", "-s", "-g", "0.8", sf])
      case r of
        Left e -> writeIORef (engMessage eng) ("FluidSynth did not start: " <> T.pack (show e))
        Right h -> do
          writeIORef (engFluid eng) (Just h)
          writeIORef (engMessage eng) "Starting FluidSynth…"
          void . forkIO $ do
            threadDelay 1500000
            shuttingDown <- readIORef (engShuttingDown eng)
            unless shuttingDown $ do
              getProcessExitCode h >>= \case
                Just _ -> writeIORef (engMessage eng) "FluidSynth exited before its MIDI output became available."
                Nothing -> do
                  rescanMidi eng
                  selectFluidOutput eng explicitRequest

selectFluidOutput :: Engine -> Bool -> IO ()
selectFluidOutput eng explicitRequest = do
  explicit <- readIORef (engMidiExplicit eng)
  when (explicitRequest || not explicit) $ modifyMVar_ (engMidi eng) $ \ms ->
    pure ms {msSelected = maybe (msSelected ms) Just (find (("FLUID" `T.isInfixOf`) . T.toUpper . moName) (msOutputs ms))}

-- | Where the built-in sampler stands. @siOn@ is the user's choice; the
-- sampler may still not be playing, for want of a folder or a device, which
-- @siError@ explains.
data SamplesInfo = SamplesInfo
  { siOn :: !Bool
  , siPlaying :: !Bool
  , siFolder :: !(Maybe FilePath)
  , siSounds :: !Int
  , siMissing :: ![Text]
    -- ^ Sounds the patterns played that the folder has no samples for.
  , siError :: !(Maybe Text)
  }

samplesInfo :: Engine -> IO SamplesInfo
samplesInfo eng = withMVar (engSampler eng) $ \ss -> do
  missing <- maybe (pure []) missingSounds (ssSampler ss)
  pure
    SamplesInfo
      { siOn = spOn (ssPrefs ss)
      , siPlaying = isJust (ssSampler ss)
      , siFolder = ssFolder ss
      , siSounds = maybe 0 samplerSoundCount (ssSampler ss)
      , siMissing = missing
      , siError = ssError ss
      }

withSampler :: Engine -> (Sampler -> IO ()) -> IO ()
withSampler eng act = withMVar (engSampler eng) (mapM_ act . ssSampler)

-- | Open the sampler on its folder, closing any open one first.
openIn :: SamplerState -> IO SamplerState
openIn ss = do
  mapM_ closeSampler (ssSampler ss)
  case ssFolder ss of
    Nothing -> pure ss {ssSampler = Nothing, ssError = Just "No sample folder found. Choose one, such as Dirt-Samples."}
    Just dir ->
      openSampler dir >>= \case
        Left e -> pure ss {ssSampler = Nothing, ssError = Just e}
        Right smp -> pure ss {ssSampler = Just smp, ssError = Nothing}

-- | Turn the sampler on or off, and remember the choice.
setSamples :: Engine -> Bool -> IO ()
setSamples eng on = modifyMVar_ (engSampler eng) $ \ss -> do
  let prefs = (ssPrefs ss) {spOn = on}
  saveSamplePrefs prefs
  if on
    then openIn ss {ssPrefs = prefs}
    else do
      mapM_ closeSampler (ssSampler ss)
      pure ss {ssPrefs = prefs, ssSampler = Nothing, ssError = Nothing}

-- | Play samples from a folder from now on, and remember it.
setSampleFolder :: Engine -> FilePath -> IO ()
setSampleFolder eng dir = modifyMVar_ (engSampler eng) $ \ss -> do
  let prefs = SamplePrefs {spFolder = Just dir, spOn = True}
  saveSamplePrefs prefs
  openIn ss {ssPrefs = prefs, ssFolder = Just dir}

-- | Use a project's bundled bank while it is open, without changing the
-- user's configured sample folder. Clearing it restores that folder or the
-- system Dirt-Samples bank.
setProjectSampleFolder :: Engine -> Maybe FilePath -> IO ()
setProjectSampleFolder eng projectFolder = modifyMVar_ (engSampler eng) $ \ss -> do
  configured <- maybe defaultSampleFolder (pure . Just) (spFolder (ssPrefs ss))
  let next = ss {ssFolder = projectFolder `orFolder` configured}
  if spOn (ssPrefs ss)
    then openIn next
    else pure next {ssSampler = Nothing, ssError = Nothing}
  where
    orFolder (Just folder) _ = Just folder
    orFolder Nothing fallback = fallback

engineMessage :: Engine -> IO Text
engineMessage eng = readIORef (engMessage eng)
