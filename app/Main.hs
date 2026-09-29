module Main (main) where

import Codec.Picture (Image (..), PixelRGBA8, writePng)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Vector.Storable qualified as V
import Lunar.Engine (newEngine, setPlaying, shutdownEngine)
import Lunar.Model (demoSong, songCps)
import Data.List (find)
import Lunar.UI (AppEnv (..), lunarApp, newAppEnv, workspaceName)
import Lunar.UI.Palette (lunarTheme)
import NanoUI
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlOptions (..), defaultSdlOptions, runSdlApp)
import System.Environment (getArgs)
import Data.Maybe (isJust, fromMaybe)
import Text.Read (readMaybe)

-- | @lunar-cycles [--play] [--view arrange|pattern|code|mixer] [--screenshot FILE
-- [--after SECONDS]]@. @--view@ picks the view the window opens on. With
-- @--screenshot@ the window saves itself as a PNG once the given time has
-- passed, then closes.
main :: IO ()
main = do
  args <- getArgs
  let flag f = f `elem` args
      opt f = case dropWhile (/= f) args of
        _ : v : _ -> Just v
        _ -> Nothing
      shotAfter = fromMaybe 1.5 (opt "--after" >>= readMaybe) :: Double
      dimension name fallback = max 360 (fromMaybe fallback (opt name >>= readMaybe))
  bracket (newEngine (songCps demoSong)) shutdownEngine $ \eng -> do
    env0 <- newAppEnv eng
    let env = maybe env0 (\w -> env0 {envStartView = w}) (opt "--view" >>= \v -> find ((== v) . workspaceName) [minBound .. maxBound])
    when (flag "--play") (setPlaying eng True)
    started <- newIORef Nothing
    taken <- newIORef False
    let view = do
          lunarApp env
          case opt "--screenshot" of
            Nothing -> pure ()
            Just path -> do
              t <- uiTime
              t0 <- liftIO (atomicModifyIORef' started (\s -> (Just (maybe t id s), maybe t id s)))
              done <- liftIO (readIORef taken)
              if t - t0 >= shotAfter && not done
                then do
                  liftIO (writeIORef taken True)
                  requestScreenshot (mapM_ (savePng path))
                else wakeAfter 0.1
              when done quitUi
    runSdlApp
      defaultSdlOptions
        { sdlWindowSettings =
            defaultWindowSettings
              { wsTitle = "LunarCycles"
               , wsSize = Size (dimension "--width" 1600) (dimension "--height" 980)
               , wsMinSize = Just (Size 400 600)
               , wsMode = if isJust (opt "--screenshot") then Hidden else Windowed
               }
        , sdlAppTheme = Just lunarTheme
        , sdlAppFont = FontSearch ["Input Sans Condensed Regular", "Noto Sans Regular", "sans-serif"]
        , sdlAppMonoFont = FontSearch ["Input Mono Regular", "DejaVu Sans Mono", "monospace"]
        , sdlAppFontSize = 14
        }
      view

savePng :: FilePath -> Screenshot -> IO ()
savePng path shot = do
  let px = screenshotPixels shot
      img = Image (rgbaWidth px) (rgbaHeight px) (V.fromList (BS.unpack (rgbaBytes px))) :: Image PixelRGBA8
  writePng path img
