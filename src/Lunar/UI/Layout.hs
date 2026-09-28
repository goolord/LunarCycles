-- | Keeping the pane arrangement between runs: the pane grid's split tree,
-- written as it shows itself.
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

layoutFile :: IO FilePath
layoutFile = (</> "layout") <$> getXdgDirectory XdgConfig "lunar-cycles"

-- | The arrangement saved last time, if there is a readable one.
loadLayout :: IO (Maybe GridNode)
loadLayout = do
  r <- try @SomeException (readFile' =<< layoutFile)
  pure (either (const Nothing) readMaybe r)
  where
    readFile' path = do
      s <- readFile path
      length s `seq` pure s

saveLayout :: GridNode -> IO ()
saveLayout layout = void . try @SomeException $ do
  path <- layoutFile
  dir <- getXdgDirectory XdgConfig "lunar-cycles"
  createDirectoryIfMissing True dir
  writeFile path (show layout <> "\n")

forgetLayout :: IO ()
forgetLayout = void . try @SomeException $ removeFile =<< layoutFile
