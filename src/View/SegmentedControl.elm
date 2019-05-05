module View.SegmentedControl exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Maybe.Extra
import View.Theme exposing (theme)


segmentedControlButton : List (SegmentedControlOption id) -> SegmentedControlItem id msg -> Html msg
segmentedControlButton options (SegmentedControlItem id msg { text, sup, image, info }) =
    let
        checked =
            List.member (SelectedIdx id) options

        titleAttr =
            info
                |> Maybe.map (A.title >> List.singleton)
                |> Maybe.withDefault []
    in
    styled S.button
        [ displayFlex
        , alignItems baseline
        , backgroundColor <|
            if checked then
                theme.active

            else
                rgba 0 0 0 0
        , color <|
            if checked then
                theme.textOnDark

            else
                theme.textOnDark
        , padding2 (px 2) <| px 10
        , paddingBottom <|
            if Maybe.Extra.isJust sup then
                px 2.5

            else
                px 0
        , theme.font
        , fontSize <| rem 1
        , outline none
        , cursor <|
            if checked then
                default

            else
                pointer
        , borderStyle solid
        , borderColor <| theme.secondaryOnDark
        , borderWidth4 (px 0) (px 0) (px 0) (px 0.5)
        , marginLeft <| px -1
        , hover <|
            if checked then
                []

            else
                [ backgroundColor <| rgb 70 70 70
                , boxShadow4 (px 0) (px 0) (px 9) (rgba 0 0 0 0.5)
                ]
        , active <|
            if checked then
                []

            else
                [ backgroundColor <| rgb 100 100 100 ]
        , focus <| [ outline none ]
        , textShadow4 (px 0) (px 0) (px 7) (rgba 0 0 0 0.2)
        ]
        ([ E.onClick (msg id) ] ++ titleAttr)
        ([ Maybe.map
            (\src ->
                styled S.img
                    [ width auto, height <| px 18, color theme.secondaryOnDark ]
                    [ A.src src
                    ]
                    []
            )
            image
         , [ Maybe.map S.text text, Maybe.map (\s -> S.sup [] [ S.text s ]) sup ]
            |> List.filterMap identity
            |> S.span []
            |> Just
         ]
            |> List.filterMap identity
        )


type SegmentedControlItem id msg
    = SegmentedControlItem
        id
        (id -> msg)
        { text : Maybe String
        , image : Maybe String
        , sup : Maybe String
        , info : Maybe String
        }


type SegmentedControlOption id
    = SelectedIdx id


segmentedControl : List (SegmentedControlOption id) -> List (SegmentedControlItem id msg) -> Html msg
segmentedControl options items =
    styled S.div
        [ borderRadius <| px 8
        , borderWidth <| px 0.5
        , borderStyle solid
        , borderColor <| theme.secondaryOnDark
        , overflow <| hidden
        , display inlineBlock
        , whiteSpace noWrap
        , height <| px 30
        , displayFlex
        , alignItems stretch
        ]
        []
        (items |> List.map (segmentedControlButton options))
