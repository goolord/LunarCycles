module Lunar.UI.Control
  ( selectField
  , wrappedText
  ) where

import Control.Monad (void)
import Data.Text (Text)
import NanoUI

-- | A select whose full option shows in a tooltip, since a narrow select
-- cuts its label short.
selectField :: Float -> [Text] -> Int -> NanoUI Int
selectField width options current = do
  let full = case drop (max 0 current) options of
        t : _ -> t
        [] -> ""
  ((_, picked), _) <- withTooltip
    (selectWith' (fixedW width . minH 36 . fontSize 14 . alignMid) options current)
    (wrappedText (fixedW 280 . fontSize 13) full)
  pure picked

wrappedText :: (Layout -> Layout) -> Text -> NanoUI ()
wrappedText layout text = void (richTextWith (layout . fillW) [inlineText text])
