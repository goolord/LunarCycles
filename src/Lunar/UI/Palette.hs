-- | The instrument palette and the track colours shared by every view.
module Lunar.UI.Palette
  ( lunarTheme
  , trackColor
  , monoSmall
  ) where

import NanoUI

-- | A harbour at night. Three depths of grey-blue slate separate the
-- workspace, the pane surfaces and the raised controls; every control
-- carries a rim at nearly 5:1 against its pane so it reads as something to
-- press. Moon yellow is kept for what moves or matters most: transport, the
-- playhead and keyboard focus.
lunarTheme :: Theme
lunarTheme =
  let water = colorRGB 8 15 21
      hull = colorRGB 23 36 48
      cap = colorRGB 43 61 80
      capHover = colorRGB 54 76 98
      rim = colorRGB 120 146 169
      inputRim = colorRGB 90 114 137
      inputHover = colorRGB 14 24 33
      floating = colorRGB 30 45 60
      rule = colorRGB 39 55 71
      text = colorRGB 232 237 241
      mist = colorRGB 159 176 191
      moon = colorRGB 242 210 122
      glass = colorRGB 127 209 178
      harbour = colorRGB 130 188 235
      coral = colorRGB 240 145 111
      amber = colorRGB 255 190 102
      alarm = colorRGB 255 123 123
      control = foreground text . borderColor rim . cornerRadius 6 . borderWidth 1
   in defaultTheme
        { themeWindow = water
        , themePanel = (background hull . foreground text . borderWidth 0 . cornerRadius 0) (themePanel defaultTheme)
        , themeButton = (control . background cap . hoverBackground capHover . pressBackground hull) (themeButton defaultTheme)
        , themeInput = (control . borderColor inputRim . background water . hoverBackground inputHover . pressBackground water) (themeInput defaultTheme)
        , themeFloatingWindow = (control . background floating . cornerRadius 8) (themeFloatingWindow defaultTheme)
        , themePopup = (control . background floating . cornerRadius 8) (themePopup defaultTheme)
        , themeSeparator = rule
        , themeAccent = moon
        , themeMuted = mist
        , themeRed = alarm
        , themeOrange = coral
        , themeYellow = amber
        , themeGreen = glass
        , themePurple = harbour
        , themeSuccess = glass
        , themeWarning = amber
        , themeDanger = alarm
        , themeOnAccent = water
        , themeFocusRing = moon
        , themeSelection = withAlpha harbour 0.3
        , themeLink = glass
        , themeShadow = colorRGBA 3 7 14 220
        }

-- | The colour of the track at a position in the song: sea glass, harbour
-- blue, buoy coral and kelp, far enough apart in hue to tell four lanes
-- apart, each above 6.5:1 on a pane.
trackColor :: Theme -> Int -> Color
trackColor th i = cycle palette !! max 0 i
  where
    palette = [themeGreen th, themePurple th, themeOrange th, kelp]
    kelp = colorRGB 188 208 114

-- | Small monospaced text, for code fragments inside the editors.
monoSmall :: Layout -> Layout
monoSmall = fontMono . fontSize 12
