module View.Theme exposing (..)

import Css exposing (Color, Style, rgba, rgb, fontFamilies)


theme :
    { background : Color
    , text : Color
    , secondary : Color
    , line : Color
    , darkLine : Color
    , clear : Color
    , inputBackground : Color
    , font : Style
    }
theme =
    { background = rgb 240 240 240
    , text = rgba 10 10 10 0.85
    , secondary = rgb 250 240 230
    , line = rgb 211 211 211
    , darkLine = rgb 150 150 150
    , clear = rgba 0 0 0 0
    , inputBackground = rgba 255 255 255 0.5
    , font = fontFamilies [ "cm" ]
    }
