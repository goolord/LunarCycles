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
import Data.List (find, nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lunar.Model (Clip, Param, ParamSetting, Song, Source, Transform)
import Lunar.Model qualified as M
import System.FilePath (takeBaseName, takeExtension, (<.>))
import Text.Read (readMaybe)

projectExtension :: String
projectExtension = "lunar"

header :: Text
header = "LunarCycles song 2"

-- | Songs written before channels were shared, when each sequence held
-- tracks of its own.
headerV1 :: Text
headerV1 = "LunarCycles song 1"

songToText :: Song -> Text
songToText song = T.unlines [header, T.pack (show song)]

songFromText :: Text -> Either Text Song
songFromText txt = case T.lines txt of
  h : body
    | T.strip h == header -> parsed id body
    | T.strip h == headerV1 -> parsed fromV1 body
  _ -> Left "This is not a LunarCycles song."
  where
    parsed :: Read a => (a -> Song) -> [Text] -> Either Text Song
    parsed convert body = maybe (Left "The song in this file could not be read.") (Right . repair . convert) (readMaybe (T.unpack (T.unlines body)))
    -- A song always has a sequence to edit.
    repair s
      | null (M.songSequences s) = s {M.songSequences = [M.newSequence 1 "sequence"], M.songPlaylist = []}
      | otherwise = s

-- | The first file format, read by the derived 'Read' of these types, which
-- is why they carry the names the old ones had.
data SongV1 = Song
  { songCps :: !Double
  , songSequences :: ![Sequence]
  , songPlaylist :: ![Clip]
  }
  deriving (Read)

data Sequence = Sequence
  { seqId :: !Int
  , seqName :: !Text
  , seqCycles :: !Int
  , seqTracks :: ![Track]
  }
  deriving (Read)

data Track = Track
  { trackId :: !Int
  , trackName :: !Text
  , trackSound :: !Text
  , trackSource :: !Source
  , trackChain :: ![(Int, Transform)]
  , trackParams :: !(Map Param ParamSetting)
  , trackMuted :: !Bool
  , trackSolo :: !Bool
  }
  deriving (Read)

-- | Tracks with the same name and sound become one channel, taking the
-- settings of the first; each sequence keeps its rhythms as the channel's
-- parts.
fromV1 :: SongV1 -> Song
fromV1 old =
  M.Song
    { M.songCps = songCps old
    , M.songChannels = zipWith channel [1 ..] firsts
    , M.songSequences = map sequence' (songSequences old)
    , M.songPlaylist = songPlaylist old
    }
  where
    key t = (trackName t, trackSound t)
    firsts = nubBy (\a b -> key a == key b) (concatMap seqTracks (songSequences old))
    channel cid t =
      (M.newChannel cid (trackName t) (trackSound t))
        { M.chanParams = trackParams t
        , M.chanMuted = trackMuted t
        , M.chanSolo = trackSolo t
        }
    channelOf t = maybe 0 fst (find ((== key t) . key . snd) (zip [1 ..] firsts))
    sequence' sq =
      M.Sequence
        { M.seqId = seqId sq
        , M.seqName = seqName sq
        , M.seqCycles = seqCycles sq
        , M.seqParts = Map.fromList [(channelOf t, M.Part (trackSource t) (trackChain t)) | t <- seqTracks sq]
        }

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
