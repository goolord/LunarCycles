-- | The instrument palette and the track colours shared by every view.
module Lunar.UI.Palette
  ( lunarTheme
  , trackColor
  , monoSmall
  ) where

import NanoUI

-- | The moon's surface. Three depths of warm, near-neutral grey (dark mare
-- basalt, regolith, highland) separate the workspace, the pane surfaces and
-- the raised controls, low enough in chroma that colour only appears where
-- it means something. Harvest moon amber is kept for what moves or matters
-- most: transport, the playhead and keyboard focus.
lunarTheme :: Theme
lunarTheme =
  let mare = colorRGB 20 19 18
      regolith = colorRGB 36 35 33
      highland = colorRGB 54 52 49
      highlandHover = colorRGB 66 64 60
      rim = colorRGB 118 114 108
      inputRim = colorRGB 104 100 95
      inputHover = colorRGB 27 26 24
      floating = colorRGB 44 43 40
      rule = colorRGB 50 48 45
      text = colorRGB 226 222 214
      dust = colorRGB 164 159 150
      harvest = colorRGB 236 163 76
      lichen = colorRGB 146 178 118
      tide = colorRGB 110 168 160
      blood = colorRGB 206 114 92
      straw = colorRGB 204 186 122
      alarm = colorRGB 232 96 86
      control = foreground text . borderColor rim . cornerRadius 6 . borderWidth 1
   in defaultTheme
        { themeWindow = mare
        , themePanel = (background regolith . foreground text . borderWidth 0 . cornerRadius 0) (themePanel defaultTheme)
        , themeButton = (control . background highland . hoverBackground highlandHover . pressBackground regolith) (themeButton defaultTheme)
        , themeInput = (control . borderColor inputRim . background mare . hoverBackground inputHover . pressBackground mare) (themeInput defaultTheme)
        , themeFloatingWindow = (control . background floating . cornerRadius 8) (themeFloatingWindow defaultTheme)
        , themePopup = (control . background floating . cornerRadius 8) (themePopup defaultTheme)
        , themeSeparator = rule
        , themeAccent = harvest
        , themeMuted = dust
        , themeRed = alarm
        , themeOrange = blood
        , themeYellow = straw
        , themeGreen = lichen
        , themePurple = tide
        , themeSuccess = lichen
        , themeWarning = straw
        , themeDanger = alarm
        , themeOnAccent = mare
        , themeFocusRing = harvest
        , themeSelection = withAlpha harvest 0.25
        , themeLink = tide
        , themeShadow = colorRGBA 0 0 0 200
        }

-- | The colour of the track at a position in the song: lichen, tide, blood
-- moon and straw, far enough apart in hue to tell four lanes apart and
-- muted enough that a full timeline stays calm. Each is at least 4.6:1 on a
-- pane.
trackColor :: Theme -> Int -> Color
trackColor th i = cycle palette !! max 0 i
  where
    palette = [themeGreen th, themePurple th, themeOrange th, themeYellow th]

-- | Small monospaced text, for code fragments inside the editors.
monoSmall :: Layout -> Layout
monoSmall = fontMono . fontSize 12
