module View.Theme exposing (..)

import Css exposing (Color, Style, fontFamilies, rgb, rgba)


theme :
    { background : Color
    , backgroundDark : Color
    , text : Color
    , error : Color
    , errorText : Color
    , successText : Color
    , textOnDark : Color
    , secondary : Color
    , secondaryOnDark : Color
    , line : Color
    , darkLine : Color
    , lightLine : Color
    , clear : Color
    , inputBackground : Color
    , active : Color
    , cmFont : Style
    , ktFont : Style
    , font : Style
    }
theme =
    { background = rgb 240 240 240
    , backgroundDark = rgb 38 50 56
    , text = rgb 40 40 40
    , error = rgb 253 220 229
    , errorText = rgb 230 55 86
    , successText = rgb 75 181 67
    , textOnDark = rgb 247 247 247
    , secondary = rgb 50 50 50
    , secondaryOnDark = rgb 222 222 222
    , line = rgba 180 180 180 0.9
    , darkLine = rgb 140 140 140
    , lightLine = rgb 185 185 185
    , clear = rgba 0 0 0 0
    , inputBackground = rgb 249 249 249
    , active = rgb 33 150 243
    , cmFont = fontFamilies [ "cm" ]
    , ktFont = fontFamilies [ "kt" ]
    , font = fontFamilies [ "cm", "kt" ]
    }
