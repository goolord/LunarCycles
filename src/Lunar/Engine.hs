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
  , engineMessage
  ) where

import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad
import Data.IORef
import Data.List (find, isPrefixOf, isSuffixOf, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
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
import Lunar.Compile
import Lunar.Midi
import Lunar.Sampler
import Sound.Tidal.ID (ID (..))
import Sound.Tidal.Pattern (EventF (..), eventHasOnset, silence, wholeStart)
import Sound.Tidal.Stream qualified as Tidal
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory)
import System.Process (ProcessHandle, getProcessExitCode, spawnProcess, terminateProcess)

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
  , engFluid :: !(IORef (Maybe ProcessHandle))
  , engMessage :: !(IORef Text)
  , engThreads :: !(IORef [ThreadId])
  }

-- | How far ahead of the clock notes are handed to MIDI, in seconds.
lookahead :: Double
lookahead = 0.1

newEngine :: Double -> IO Engine
newEngine cps = do
  now <- getMonotonicTime
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
      <*> newIORef Nothing
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
  mapM_ killThread =<< readIORef (engThreads eng)
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

schedulerLoop :: Engine -> IO ()
schedulerLoop eng = forever $ do
  threadDelay 4000
  now <- getMonotonicTime
  t <- readIORef (engTransport eng)
  when (tPlaying t) $ do
    let cNow = cycleAt t now
        horizon = cNow + lookahead * tCps t
    from <- atomicModifyIORef' (engCursor eng) $ \c ->
      (horizon, if c < cNow - 0.25 || c > horizon then cNow else c)
    when (horizon > from) $ do
      comps <- readIORef (engTracks eng)
      let due =
            [ (c, e, (on - cNow) / tCps t)
            | c <- comps
            , e <- eventsIn from horizon (cPattern c)
            , eventHasOnset e
            , let on = fromRational (wholeStart e)
            , on >= from && on < horizon
            ]
          notes = sortOn fst [(delay, n) | (c, e, delay) <- due, Just n <- [eventNote (tCps t) (cChannel c) e]]
      unless (null notes) $ modifyMVar_ (engMidi eng) $ \ms ->
        case msSelected ms of
          Nothing -> pure ms
          Just out -> foldM (sendNote (moId out)) ms notes
      unless (null due) $ withSampler eng $ \smp ->
        forM_ due $ \(_, e, delay) -> playEvent smp (now + delay) (value e)
  withMVar (engMidi eng) $ \ms -> forM_ (msSelected ms) (outputMidi . moId)

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
selectMidi eng pick = modifyMVar_ (engMidi eng) $ \ms -> do
  forM_ (msSelected ms) (allNotesOff . moId)
  pure ms {msSelected = pick >>= \nm -> find ((== nm) . moName) (msOutputs ms), msPrograms = Map.empty}

-- | Restart PortMidi so devices that appeared since start are listed. The
-- chosen device is kept by name when it is still there.
rescanMidi :: Engine -> IO ()
rescanMidi eng = modifyMVar_ (engMidi eng) $ \ms -> do
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
      pure ms {msOutputs = outs, msSelected = maybe (defaultOutput outs) Just kept, msPrograms = Map.empty}

dirtStatus :: Engine -> IO DirtStatus
dirtStatus eng = readIORef (engDirtStatus eng)

-- | Start or stop sending to SuperDirt on 127.0.0.1:57120.
setDirt :: Engine -> Bool -> IO ()
setDirt eng on = do
  status <- readIORef (engDirtStatus eng)
  case (on, status) of
    (True, DirtOff) -> start
    (True, DirtFailed _) -> start
    (False, DirtOn) -> do
      readIORef (engDirt eng) >>= mapM_ Tidal.streamHush
      writeIORef (engDirtIds eng) []
      writeIORef (engDirtStatus eng) DirtOff
    _ -> pure ()
  where
    start = do
      writeIORef (engDirtStatus eng) DirtStarting
      existing <- readIORef (engDirt eng)
      void . forkIO $ do
        r <- case existing of
          Just st -> pure (Right st)
          Nothing ->
            try @SomeException $
              Tidal.startTidal
                (Tidal.superdirtTarget {Tidal.oLatency = 0.1, Tidal.oAddress = "127.0.0.1", Tidal.oPort = 57120})
                Tidal.defaultConfig {Tidal.cVerbose = False, Tidal.cCtrlListen = False}
        case r of
          Left e -> writeIORef (engDirtStatus eng) (DirtFailed (T.pack (show e)))
          Right st -> do
            writeIORef (engDirt eng) (Just st)
            t <- readIORef (engTransport eng)
            Tidal.streamSetCPS st (toRational (tCps t))
            now <- getMonotonicTime
            Tidal.streamSetCycle st (toRational (cycleAt t now))
            writeIORef (engDirtStatus eng) DirtOn
            pushDirt eng

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
      let current = [("lunar" <> show (cChannel c), cPattern c) | playing, c <- comps]
      forM_ current $ \(i, p) -> Tidal.streamReplace st (ID i) p
      forM_ (filter (`notElem` map fst current) old) $ \i -> Tidal.streamReplace st (ID i) silence
      writeIORef (engDirtIds eng) (map fst current)
    _ -> pure ()

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
startFluid eng sf = do
  running <- fluidRunning eng
  unless running $ do
    r <- try @SomeException (spawnProcess "fluidsynth" ["-a", "pipewire", "-m", "alsa_seq", "-i", "-s", "-g", "0.8", sf])
    case r of
      Left e -> writeIORef (engMessage eng) ("FluidSynth did not start: " <> T.pack (show e))
      Right h -> do
        writeIORef (engFluid eng) (Just h)
        writeIORef (engMessage eng) "Starting FluidSynth…"
        void . forkIO $ do
          threadDelay 1500000
          rescanMidi eng
          modifyMVar_ (engMidi eng) $ \ms ->
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

engineMessage :: Engine -> IO Text
engineMessage eng = readIORef (engMessage eng)
