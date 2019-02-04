module View.Lambda.ExpressionText exposing (..)

import Html.Styled as S exposing (Html, styled)
import Css exposing (..)
import View.Theme exposing (theme)


lambdaExprText value =
    styled S.div
        [ color theme.text
        , backgroundColor theme.clear
        , fontSize <| rem 1
        , theme.font
        , fontWeight normal
        , margin2 (rem 0) (rem 0.3)
        ]
        []
        [ S.text value ]
