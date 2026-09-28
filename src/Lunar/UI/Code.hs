-- | The generated Tidal code, highlighted, with the selected track's lines
-- tinted in its colour.
module Lunar.UI.Code
  ( codeView
  ) where

import Control.Monad (forM_, void)
import Data.Text qualified as T
import Lunar.Codegen (Line, Tok (..), TokClass (..))
import Lunar.UI.Control (stripe)
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
      -- The selected track's lines carry its colour down their left edge.
      marked tag = case (selected, tag) of
        (Just (tid, col), Just t) | t == tid -> Just col
        _ -> Nothing
      text line = void (richTextWith (fillW . fontMono . fontSize 13) (map piece line))
  scrollWith (fillW . fillH) $
    columnWith (tight . gap 0 . fillW) $
      forM_ (zip [0 :: Int ..] code) $ \(i, (tag, line)) -> withKey i $
        if all (T.null . T.strip . tokText) line
          then spacer (Fixed 1) (Fixed 8)
          else case marked tag of
            Just col -> rowWith (tight . fillW . gap 0) $ do
              stripe 3 0 col
              styled (panelStyle (background (withAlpha col 0.12) . borderWidth 0 . cornerRadius 0)) $
                panelWith (tight . fillW . padXY 8 1) (text line)
            Nothing -> columnWith (tight . fillW . padLRTB 11 8 1 1) (text line)
