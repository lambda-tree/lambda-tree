module View.Lambda.ExpressionInput exposing (..)

import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Html.Styled as S exposing (Html, styled)
import Css exposing (..)
import View.Theme exposing (theme)


lambdaExprInput alignRight value onInput =
    styled S.input
        [ color <| theme.text
        , backgroundColor <| theme.inputBackground
        , fontSize <| rem 1
        , theme.font
        , width <| pct 100
        , padding2 (rem 0.3) (rem 0.6)
        , borderRadius <| rem 0.5
        , borderWidth <| rem 0.1
        , borderStyle solid
        , border <| rem 0
        , property "-webkit-appearance" "none"
        , property "-moz-appearance" "none"
        , property "-ms-appearance" "none"
        , property "-o-appearance" "none"
        , property "appearance" "none"
        , outline none
        , boxShadow none
        , property "direction" <|
            if alignRight then
                "ltr"
            else
                "ltr"
        ]
        [ A.value value, E.onInput onInput ]
        []
