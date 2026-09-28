-- | Keeping each view's pane arrangement between runs: the pane grid's
-- split tree, written as it shows itself, one file per view.
module Lunar.UI.Layout
  ( loadLayout
  , saveLayout
  , forgetLayout
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import NanoUI (GridNode)
import System.Directory
  ( XdgDirectory (XdgConfig)
  , createDirectoryIfMissing
  , getXdgDirectory
  , removeFile
  )
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | Where the arrangement of the named view is kept.
layoutFile :: String -> IO FilePath
layoutFile view = (</> ("layout-" <> view)) <$> getXdgDirectory XdgConfig "lunar-cycles"

-- | The arrangement saved last time, if there is a readable one.
loadLayout :: String -> IO (Maybe GridNode)
loadLayout view = do
  r <- try @SomeException (readFile' =<< layoutFile view)
  pure (either (const Nothing) readMaybe r)
  where
    readFile' path = do
      s <- readFile path
      length s `seq` pure s

saveLayout :: String -> GridNode -> IO ()
saveLayout view layout = void . try @SomeException $ do
  path <- layoutFile view
  dir <- getXdgDirectory XdgConfig "lunar-cycles"
  createDirectoryIfMissing True dir
  writeFile path (show layout <> "\n")

forgetLayout :: String -> IO ()
forgetLayout view = void . try @SomeException $ removeFile =<< layoutFile view
