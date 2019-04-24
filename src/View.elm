module View exposing (..)

import Css exposing (..)
import Css.Global exposing (body, global, selector)
import Html.Styled as S exposing (Html, styled)
import Message exposing (..)
import Model exposing (..)
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
                [ property "src" "url(/fonts/cmunrm.ttf)"
                , fontStyle normal
                , theme.cmFont
                ]
            , selector "@font-face"
                [ property "src" "url(/fonts/cmunti.ttf)"
                , fontStyle italic
                , theme.cmFont
                ]
            , selector "@font-face"
                [ property "src" "url(/fonts/KaTeX_Main-Regular.ttf)"
                , fontStyle normal
                , theme.ktFont
                ]
            ]
        , mainContent model
        ]
