-- | The generated Tidal code, highlighted, with the selected track's lines
-- tinted in its colour.
module Lunar.UI.Code
  ( codeView
  ) where

import Control.Monad (forM_, void)
import Data.Text qualified as T
import Lunar.Codegen (Line, Tok (..), TokClass (..))
import Lunar.UI.Palette (withAlpha)
import NanoUI

codeView :: Maybe (Int, Color) -> [(Maybe Int, Line)] -> NanoUI ()
codeView selected code = do
  th <- uiTheme
  let colour = \case
        TkFunc -> themePurple th
        TkString -> themeGreen th
        TkNumber -> themeOrange th
        TkOp -> themeMuted th
        TkComment -> themeMuted th
        TkPlain -> styleFg (themePanel th)
      piece t = inlineWith (fontMono . fontSize 13 . fontColor (colour (tokClass t)) . commentStyle (tokClass t)) (tokText t)
      commentStyle TkComment = fontItalic
      commentStyle _ = id
      clear = withAlpha (themeWindow th) 0
      -- The selected track's lines carry its colour down their left edge.
      marked tag = case (selected, tag) of
        (Just (tid, col), Just t) | t == tid -> Just col
        _ -> Nothing
      fill c = styled (panelStyle (background c . borderWidth 0 . cornerRadius 0))
  scrollWith (fillW . fillH) $
    columnWith (tight . gap 0 . fillW) $
      forM_ (zip [0 :: Int ..] code) $ \(i, (tag, line)) -> withKey i $
        if all (T.null . T.strip . tokText) line
          then spacer (Fixed 1) (Fixed 8)
          else rowWith (tight . fillW . gap 0) $ do
            fill (maybe clear id (marked tag)) $ panelWith (fixedW 3 . fillH . tight) (pure ())
            fill (maybe clear (`withAlpha` 0.12) (marked tag)) $
              panelWith (tight . fillW . padXY 8 1) $ void (richTextWith (fillW . fontMono . fontSize 13) (map piece line))
