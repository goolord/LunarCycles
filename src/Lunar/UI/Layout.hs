-- | Keeping the pane arrangement between runs.
--
-- nano-ui's pane grid keeps its split tree to itself, but a split tree
-- always tiles its area with straight cuts, so the tree can be read back
-- from where the panes are: find a line that crosses the grid without
-- entering any pane, split there, and do the same on each side.
module Lunar.UI.Layout
  ( SavedLayout (..)
  , layoutFromRects
  , toGridNode
  , loadLayout
  , saveLayout
  , forgetLayout
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.List (nub, partition, sort)
import Data.Word (Word64)
import NanoUI (GridAxis (..), GridNode (..), Rect (..))
import System.Directory
  ( XdgDirectory (XdgConfig)
  , createDirectoryIfMissing
  , getXdgDirectory
  , removeFile
  )
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | A pane arrangement: a split along a vertical ('AxisV') or horizontal
-- divider, with the share of the first side, or a pane by its id.
data SavedLayout
  = LSplit !Bool !Double SavedLayout SavedLayout
    -- ^ 'True' for a vertical divider (side by side).
  | LPane !Word64
  deriving (Eq, Show, Read)

-- | The arrangement that tiles these pane rects, dividers @gutter@ wide.
-- Ratios are rounded to thousandths, so the same arrangement reads back the
-- same after a resize. 'Nothing' when the rects do not tile (a pane not yet
-- laid out, or one maximized over the others).
layoutFromRects :: Float -> [(Word64, Rect)] -> Maybe SavedLayout
layoutFromRects gutter = go
  where
    go [] = Nothing
    go [(i, _)] = Just (LPane i)
    go ps = case cutAlong True ps of
      Just r -> Just r
      Nothing -> cutAlong False ps
    cutAlong vertical ps =
      let lo (Rect x y _ _) = if vertical then x else y
          hi (Rect x y w h) = if vertical then x + w else y + h
          cuts = sort (nub [hi r | (_, r) <- ps])
          valid c =
            let (a, b) = partition (\(_, r) -> hi r <= c + 0.5) ps
             in if not (null a) && not (null b) && all (\(_, r) -> lo r >= c - 0.5) b then Just (a, b) else Nothing
       in case [(c, ab) | c <- cuts, Just ab <- [valid c]] of
            (c, (a, b)) : _ -> do
              let start = minimum [lo r | (_, r) <- ps]
                  end = maximum [hi r | (_, r) <- ps]
                  usable = end - start - gutter
                  ratio = if usable <= 0 then 0.5 else realToFrac ((c - start) / usable) :: Double
              la <- go a
              lb <- go b
              pure (LSplit vertical (fromIntegral (round (ratio * 1000) :: Int) / 1000) la lb)
            [] -> Nothing

-- | The pane grid's tree for an arrangement. Split ids start above the
-- highest pane id, so they never collide with one.
toGridNode :: SavedLayout -> GridNode
toGridNode layout = fst (build layout (maxPane layout + 1))
  where
    maxPane = \case
      LPane i -> i
      LSplit _ _ a b -> max (maxPane a) (maxPane b)
    build l next = case l of
      LPane i -> (Pane i, next)
      LSplit v r a b ->
        let (a', n1) = build a (next + 1)
            (b', n2) = build b n1
         in (Split next (if v then AxisV else AxisH) (realToFrac r) a' b', n2)

layoutFile :: IO FilePath
layoutFile = (</> "layout") <$> getXdgDirectory XdgConfig "lunar-cycles"

-- | The arrangement saved last time, if there is a readable one.
loadLayout :: IO (Maybe SavedLayout)
loadLayout = do
  r <- try @SomeException (readFile' =<< layoutFile)
  pure (either (const Nothing) readMaybe r)
  where
    readFile' path = do
      s <- readFile path
      length s `seq` pure s

saveLayout :: SavedLayout -> IO ()
saveLayout layout = void . try @SomeException $ do
  path <- layoutFile
  dir <- getXdgDirectory XdgConfig "lunar-cycles"
  createDirectoryIfMissing True dir
  writeFile path (show layout <> "\n")

forgetLayout :: IO ()
forgetLayout = void . try @SomeException $ removeFile =<< layoutFile
