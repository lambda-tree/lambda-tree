module View exposing (..)

import Css exposing (..)
import Css.Global exposing (global, body, selector)
import Html.Styled as S exposing (Html, styled)
import Model exposing (..)
import Message exposing (..)
import View.Layout exposing (mainContent)
import View.Theme exposing (theme)


view : Model -> Html Msg
view model =
    styled S.div
        [ backgroundColor theme.background
        , height <| pct 100
        , minHeight <| pct 100
        ]
        []
        [ global
            [ Css.Global.html [ height <| pct 100, minHeight <| pct 100 ]
            , body [ height <| pct 100, minHeight <| pct 100, backgroundColor theme.background, margin <| px 0 ]
            , selector "@font-face"
                [ property "src" "url(resources/fonts/cmunrm.ttf)"
                , fontStyle normal
                , theme.font
                ]
            , selector "@font-face"
                [ property "src" "url(resources/fonts/cmunti.ttf)"
                , fontStyle italic
                , theme.font
                ]
            ]
        , mainContent model
        ]
