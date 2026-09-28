-- | Songs on disk: a header line naming the format, then the song written
-- as it shows itself, the same way the pane layout is kept.
module Lunar.Project
  ( projectExtension
  , songToText
  , songFromText
  , saveSong
  , loadSong
  , projectName
  ) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lunar.Model
import System.FilePath (takeBaseName, takeExtension, (<.>))
import Text.Read (readMaybe)

projectExtension :: String
projectExtension = "lunar"

header :: Text
header = "LunarCycles song 1"

songToText :: Song -> Text
songToText song = T.unlines [header, T.pack (show song)]

songFromText :: Text -> Either Text Song
songFromText txt = case T.lines txt of
  h : body
    | T.strip h == header -> maybe (Left "The song in this file could not be read.") (Right . repair) (readMaybe (T.unpack (T.unlines body)))
  _ -> Left "This is not a LunarCycles song."
  where
    -- A song always has a sequence to edit.
    repair s
      | null (songSequences s) = s {songSequences = [newSequence 1 "sequence"], songPlaylist = []}
      | otherwise = s

-- | Write the song, adding the extension when the path has none. Returns
-- the path written.
saveSong :: FilePath -> Song -> IO (Either Text FilePath)
saveSong path song = do
  let target = if null (takeExtension path) then path <.> projectExtension else path
  r <- try @SomeException (T.writeFile target (songToText song))
  pure (either (\e -> Left ("Could not save: " <> T.pack (show e))) (const (Right target)) r)

loadSong :: FilePath -> IO (Either Text Song)
loadSong path = do
  r <- try @SomeException (T.readFile path)
  pure (either (\e -> Left ("Could not open: " <> T.pack (show e))) songFromText r)

-- | The name a song file goes by in the title bar.
projectName :: FilePath -> Text
projectName = T.pack . takeBaseName
