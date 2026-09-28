-- | The instrument palette and the track colours shared by every view.
module Lunar.UI.Palette
  ( lunarTheme
  , trackColor
  , withAlpha
  , monoSmall
  ) where

import NanoUI

lunarTheme :: Theme
lunarTheme =
  let colorRGB r g b = colorRGBA r g b 255
      water = colorRGB 24 44 50
      surface = colorRGB 33 57 64
      raised = colorRGB 45 73 80
      rule = colorRGB 67 94 100
      chalk = colorRGB 226 235 230
      secondary = colorRGB 165 184 184
      apricot = colorRGB 240 184 121
      glass = colorRGB 156 211 188
      lilac = colorRGB 181 185 232
      rose = colorRGB 245 162 166
      control = foreground chalk . borderColor rule . cornerRadius 5 . borderWidth 1
   in defaultTheme
        { themeWindow = water
        , themePanel = (background surface . foreground chalk . borderWidth 0 . cornerRadius 0) (themePanel defaultTheme)
        , themeButton = (control . background raised . hoverBackground (colorRGB 57 88 95) . pressBackground water) (themeButton defaultTheme)
        , themeInput = (control . background water . hoverBackground surface . pressBackground water) (themeInput defaultTheme)
        , themeFloatingWindow = (control . background surface . cornerRadius 8) (themeFloatingWindow defaultTheme)
        , themeSeparator = rule
        , themeAccent = apricot
        , themeMuted = secondary
        , themeRed = rose
        , themeOrange = apricot
        , themeYellow = apricot
        , themeGreen = glass
        , themePurple = lilac
        , themeSuccess = glass
        , themeWarning = apricot
        , themeDanger = rose
        , themeOnAccent = water
        , themeFocusRing = apricot
        , themeSelection = withAlpha glass 0.25
        , themeLink = glass
        , themeShadow = withAlpha water 0.5
        }

-- | The colour of the track at a position in the song.
trackColor :: Theme -> Int -> Color
trackColor th i = cycle palette !! max 0 i
  where
    palette = [themeGreen th, themePurple th, themeOrange th, styleFg (themePanel th)]

withAlpha :: Color -> Double -> Color
withAlpha c a = colorRGBA (colorR c) (colorG c) (colorB c) (round (255 * max 0 (min 1 a)))

-- | Small monospaced text, for code fragments inside the editors.
monoSmall :: Layout -> Layout
monoSmall = fontMono . fontSize 12
