-- | The built-in sampler: a folder of samples laid out as Dirt-Samples is,
-- one folder per sound (@bd@, @sn@, …) of WAV files that @n@ picks between,
-- played the way SuperDirt plays them. The mixer is C (@cbits/sampler.c@)
-- and renders on SDL's audio thread; this side finds the files, loads them
-- when first played, and turns Tidal events into voices.
module Lunar.Sampler
  ( Sampler
  , openSampler
  , offlineSampler
  , closeSampler
  , samplerFolder
  , samplerSoundCount
  , missingSounds
  , playEvent
  , cancelPending
  , renderFrames
  , SamplePrefs (..)
  , loadSamplePrefs
  , saveSamplePrefs
  , defaultSampleFolder
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad
import Data.Char (toLower)
import Data.IORef
import Data.List (isPrefixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Foreign hiding (void)
import Foreign.C
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import GHC.Clock (getMonotonicTime)
import Lunar.Compile (valueDouble, valueText)
import Sound.Tidal.Pattern (ValueMap)
import System.Directory
  ( XdgDirectory (..)
  , createDirectoryIfMissing
  , doesDirectoryExist
  , getHomeDirectory
  , getXdgDirectory
  , listDirectory
  )
import System.FilePath (takeExtension, (</>))
import Text.Read (readMaybe)

data CSampler

data CSample

foreign import ccall safe "lunar_sample_load" c_sampleLoad :: CString -> CInt -> IO (Ptr CSample)
foreign import ccall safe "lunar_sample_free" c_sampleFree :: Ptr CSample -> IO ()
foreign import ccall safe "lunar_sampler_new" c_new :: CInt -> IO (Ptr CSampler)
foreign import ccall safe "lunar_sampler_start" c_start :: Ptr CSampler -> IO CInt
foreign import ccall safe "lunar_sampler_free" c_free :: Ptr CSampler -> IO ()
foreign import ccall safe "lunar_sampler_play"
  c_play :: Ptr CSampler -> Ptr CSample -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall safe "lunar_sampler_cancel" c_cancel :: Ptr CSampler -> IO ()
foreign import ccall safe "lunar_sampler_render" c_render :: Ptr CSampler -> Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "lunar_error" c_error :: IO CString

data Sampler = Sampler
  { smpPtr :: !(Ptr CSampler)
  , smpRate :: !Int
  , smpFolder :: !FilePath
  , smpSounds :: !(Map.Map Text [FilePath])
  , smpLoaded :: !(MVar (Map.Map FilePath (Ptr CSample)))
    -- ^ Each file played so far, or a null pointer where it could not be read.
  , smpMissing :: !(IORef (Set.Set Text))
    -- ^ Sounds played that have no folder.
  }

lastError :: IO Text
lastError = T.pack <$> (peekCString =<< c_error)

-- | The sampler, playing through the default audio device.
openSampler :: FilePath -> IO (Either Text Sampler)
openSampler dir =
  offlineSampler 48000 dir >>= \case
    Left e -> pure (Left e)
    Right smp -> do
      ok <- c_start (smpPtr smp)
      if ok /= 0
        then pure (Right smp)
        else do
          e <- lastError
          closeSampler smp
          pure (Left ("No audio device: " <> e))

-- | A sampler at @hz@ that only plays into 'renderFrames'.
offlineSampler :: Int -> FilePath -> IO (Either Text Sampler)
offlineSampler hz dir =
  try @SomeException (scanFolder dir) >>= \case
    Left e -> pure (Left ("Could not read " <> T.pack dir <> ": " <> T.pack (show e)))
    Right sounds
      | Map.null sounds -> pure (Left (T.pack dir <> " has no folders of WAV files"))
      | otherwise -> do
          p <- c_new (fromIntegral hz)
          if p == nullPtr
            then Left <$> lastError
            else Right <$> (Sampler p hz dir sounds <$> newMVar Map.empty <*> newIORef Set.empty)

-- | Each subfolder that holds WAV files, by name, with its files in order.
-- A subfolder that cannot be read is left out.
scanFolder :: FilePath -> IO (Map.Map Text [FilePath])
scanFolder dir = do
  names <- listDirectory dir
  found <- forM (sort names) $ \name -> do
    let sub = dir </> name
    isDir <- doesDirectoryExist sub
    files <- if isDir then either (const []) (sort . filter isWav) <$> try @SomeException (listDirectory sub) else pure []
    pure (T.pack name, map (sub </>) files)
  pure (Map.fromList (filter (not . null . snd) found))
  where
    isWav f = map toLower (takeExtension f) == ".wav" && not ("." `isPrefixOf` f)

-- | Stop the device, then free the samples its voices read.
closeSampler :: Sampler -> IO ()
closeSampler smp = do
  c_free (smpPtr smp)
  loaded <- takeMVar (smpLoaded smp)
  mapM_ c_sampleFree (filter (/= nullPtr) (Map.elems loaded))

samplerFolder :: Sampler -> FilePath
samplerFolder = smpFolder

samplerSoundCount :: Sampler -> Int
samplerSoundCount = Map.size . smpSounds

missingSounds :: Sampler -> IO [Text]
missingSounds smp = Set.toList <$> readIORef (smpMissing smp)

-- | The file @n@ picks from a sound's folder, loaded on first use. Indexes
-- wrap around the folder, as in SuperDirt.
sampleFor :: Sampler -> Text -> Int -> IO (Maybe (Ptr CSample))
sampleFor smp name n = case Map.lookup name (smpSounds smp) of
  Nothing -> Nothing <$ modifyIORef' (smpMissing smp) (Set.insert name)
  Just files -> do
    let path = files !! (n `mod` length files)
    p <- modifyMVar (smpLoaded smp) $ \loaded -> case Map.lookup path loaded of
      Just p -> pure (loaded, p)
      Nothing -> do
        enc <- getFileSystemEncoding
        -- The file system's encoding, which round-trips names that are not UTF-8.
        p <- GHC.withCString enc path (\c -> c_sampleLoad c (fromIntegral (smpRate smp)))
        pure (Map.insert path p loaded, p)
    pure (if p == nullPtr then Nothing else Just p)

-- | Play an event's sound at a moment on 'getMonotonicTime''s clock, as
-- SuperDirt would: @n@ picks the file, @speed@ and @note@ (in semitones) set
-- the rate, and @begin@, @end@, @gain@, @pan@, @cutoff@, @resonance@ and
-- @shape@ shape the voice. The sample plays to the end of its part whatever
-- the event's length.
playEvent :: Sampler -> Double -> ValueMap -> IO ()
playEvent smp at vm =
  forM_ (valueText <$> Map.lookup "s" vm) $ \name ->
    sampleFor smp name (floor (num "n" 0)) >>= \case
      Nothing -> pure ()
      Just p -> do
        -- Taken after loading, so a first play is not late by the load.
        now <- getMonotonicTime
        c_play (smpPtr smp) p (realToFrac (at - now)) (arg rate) (param "begin" 0) (param "end" 1)
          (param "gain" 1) (param "pan" 0.5) (param "cutoff" 20000) (param "resonance" 0) (param "shape" 0)
  where
    num k d = fromMaybe d (Map.lookup k vm >>= valueDouble)
    param k d = arg (num k d)
    arg = realToFrac
    rate = num "speed" 1 * 2 ** (num "note" 0 / 12)

-- | Drop the notes handed over that have not started yet.
cancelPending :: Sampler -> IO ()
cancelPending smp = c_cancel (smpPtr smp)

-- | Mix the next @frames@ frames of an offline sampler, as left and right.
renderFrames :: Sampler -> Int -> IO [(Float, Float)]
renderFrames smp frames = allocaArray (2 * frames) $ \buf -> do
  c_render (smpPtr smp) buf (fromIntegral frames)
  pairs . map realToFrac <$> peekArray (2 * frames) buf
  where
    pairs (l : r : rest) = (l, r) : pairs rest
    pairs _ = []

-- | Whether the sampler plays, and the folder it plays from when it is not
-- where SuperCollider keeps Dirt-Samples.
data SamplePrefs = SamplePrefs {spFolder :: !(Maybe FilePath), spOn :: !Bool}
  deriving (Eq, Show, Read)

prefsFile :: IO FilePath
prefsFile = (</> "samples") <$> getXdgDirectory XdgConfig "lunar-cycles"

-- | The saved preferences; the sampler is on until turned off.
loadSamplePrefs :: IO SamplePrefs
loadSamplePrefs = do
  r <- try @SomeException (readFile' =<< prefsFile)
  pure (fromMaybe (SamplePrefs Nothing True) (either (const Nothing) readMaybe r))
  where
    readFile' path = do
      s <- readFile path
      length s `seq` pure s

saveSamplePrefs :: SamplePrefs -> IO ()
saveSamplePrefs prefs = void . try @SomeException $ do
  path <- prefsFile
  createDirectoryIfMissing True =<< getXdgDirectory XdgConfig "lunar-cycles"
  writeFile path (show prefs <> "\n")

-- | Where SuperCollider's Quarks install Dirt-Samples, if it is there.
defaultSampleFolder :: IO (Maybe FilePath)
defaultSampleFolder = do
  home <- getHomeDirectory
  xdgData <- getXdgDirectory XdgData "SuperCollider"
  let quarks base = base </> "downloaded-quarks" </> "Dirt-Samples"
  listToMaybe <$> filterM doesDirectoryExist (map quarks [xdgData, home </> "Library" </> "Application Support" </> "SuperCollider"])
