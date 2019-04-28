module View.Theme exposing (..)

import Css exposing (Color, Style, fontFamilies, rgb, rgba)


theme :
    { background : Color
    , text : Color
    , textOnDark : Color
    , secondary : Color
    , secondaryOnDark : Color
    , line : Color
    , darkLine : Color
    , clear : Color
    , inputBackground : Color
    , active : Color
    , cmFont : Style
    , ktFont : Style
    , font : Style
    }
theme =
    { background = rgb 240 240 240
    , text = rgba 10 10 10 0.85
    , textOnDark = rgba 255 255 255 0.95
    , secondary = rgb 250 240 230
    , secondaryOnDark = rgb 222 222 222
    , line = rgba 195 195 195 0.4
    , darkLine = rgb 150 150 150
    , clear = rgba 0 0 0 0
    , inputBackground = rgba 255 255 255 0.5
    , active = rgb 33 150 243
    , cmFont = fontFamilies [ "cm" ]
    , ktFont = fontFamilies [ "kt" ]
    , font = fontFamilies [ "cm", "kt" ]
    }
