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
      tint tag = case (selected, tag) of
        (Just (tid, col), Just t) | t == tid -> styled (panelStyle (background (withAlpha col 0.14) . borderWidth 0 . cornerRadius 0))
        _ -> styled (panelStyle (background (withAlpha (themeWindow th) 0) . borderWidth 0))
  scrollWith (fillW . fillH) $
    columnWith (tight . gap 0 . fillW) $
      forM_ (zip [0 :: Int ..] code) $ \(i, (tag, line)) -> withKey i $
        if all (T.null . T.strip . tokText) line
          then spacer (Fixed 1) (Fixed 8)
          else tint tag $ panelWith (tight . fillW . padXY 6 1) $ void (richTextWith (fillW . fontMono . fontSize 13) (map piece line))
