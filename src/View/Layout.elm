module View.Layout exposing (..)

import Html.Styled.Events as E
import Html.Styled as S exposing (Html, styled)
import Css exposing (..)
import Message exposing (Msg(..))
import Model exposing (ModelContent, TextKind(..))
import View.Lambda.Tree exposing (drawTree)
import View.Theme exposing (theme)


mainContent model =
    styled S.div
        [ displayFlex
        , flex <| auto
        , alignItems <| stretch
        , justifyContent stretch
        , height <| pct 100
        , minHeight <| pct 100
        ]
        []
        [ leftColumn model.zoomLevel
            [ treeContainer
                [ drawTree model.tree
                ]
            ]
        , rightColumn
            [ scroller
                [ styled S.button [ margin <| rem 0.5 ] [ E.onClick ClickedMsg ] [ S.text "Click" ]
                , styled S.button [ margin <| rem 0.5 ] [ E.onClick ZoomIn ] [ S.text "Zoom In" ]
                , styled S.button [ margin <| rem 0.5 ] [ E.onClick ZoomOut ] [ S.text "Zoom Out" ]
                ]
            ]
        ]


leftColumn zoomLevel children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 8
        , alignItems flexStart
        , justifyContent flexStart
        , overflow auto
        , whiteSpace noWrap
        , property "zoom" <| String.fromFloat zoomLevel ++ "%"
        ]
        []
        children


rightColumn children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 1
        , alignItems stretch
        , justifyContent flexStart
        , overflow auto
        , whiteSpace noWrap
        , borderStyle solid
        , borderWidth4 (px 0) (px 0) (px 0) (px 1)
        , borderColor <| theme.line
        ]
        []
        children


treeContainer children =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems center
        ]
        []
        children


scroller children =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems center
        , overflow scroll
        ]
        []
        children
