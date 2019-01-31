module Main exposing (..)

import Browser
import Debug
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Css exposing (..)
import Css.Global exposing (global, body, selector)
import Html.Styled as S exposing (Html, Attribute, styled, toUnstyled, Attribute, div)


-- CSS Constants


cmFont =
    fontFamilies [ "cm" ]



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }



-- MODEL


type alias Model =
    {}


init : Model
init =
    Model



-- UPDATE


type Msg
    = TextChangedMsg
    | ClickedMsg


update : Msg -> Model -> Model
update msg model =
    case Debug.log "update :: Msg" msg of
        TextChangedMsg ->
            {}

        ClickedMsg ->
            {}



-- VIEW


theme :
    { background : Color
    , text : Color
    , secondary : Color
    , line : Color
    , clear : Color
    , inputBackground : Color
    }
theme =
    { background = rgb 240 240 240
    , text = rgba 10 10 10 0.85
    , secondary = rgb 250 240 230
    , line = rgb 211 211 211
    , clear = rgba 0 0 0 0
    , inputBackground = rgba 255 255 255 0.5
    }


background : List (Attribute msg) -> List (Html msg) -> Html msg
background =
    styled div
        [ backgroundColor theme.background
        , height <| pct 100
        ]


lambdaExprInput value onInput =
    styled S.input
        [ color <| theme.text
        , backgroundColor <| theme.inputBackground
        , fontSize <| rem 1
        , cmFont
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
        ]
        [ A.value value, E.onInput onInput ]
        []


lambdaExprText value =
    styled S.div
        [ color theme.text
        , backgroundColor theme.clear
        , fontSize <| rem 1
        , cmFont
        , fontWeight normal
        , margin2 (rem 0) (rem 0.3)
        ]
        []
        [ S.text value ]


initialExprInput =
    lambdaExprInput "Λα. λf: α → α. λx: α. f (f x)" (\x -> TextChangedMsg)


proofCell : NodeContent -> S.Html Msg
proofCell content =
    S.div
        []
        [ styled S.div
            [ displayFlex, flexShrink <| int 0, alignItems center, minWidth <| rem 20, margin <| rem 0.5 ]
            []
            [ styled S.div [ displayFlex, flex <| int 1 ] [] [ lambdaExprInput content.ctx (\x -> TextChangedMsg) ]
            , lambdaExprText "⊢"
            , styled S.div [ displayFlex, flex <| int 2 ] [] [ lambdaExprInput content.term (\x -> TextChangedMsg) ]
            , lambdaExprText ":"
            , styled S.div [ displayFlex, flex <| int 1 ] [] [ lambdaExprInput content.ty (\x -> TextChangedMsg) ]
            ]
        ]


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
                , cmFont
                ]
            , selector "@font-face"
                [ property "src" "url(resources/fonts/cmunti.ttf)"
                , fontStyle italic
                , cmFont
                ]
            ]
        , mainContent
        ]


mainContent =
    styled S.div
        [ displayFlex
        , flex <| auto
        , alignItems <| stretch
        , justifyContent stretch
        , height <| pct 100
        , minHeight <| pct 100
        ]
        []
        [ leftColumn
            [ scroller
                [ proofCell exampleContent ]
            ]
        , rightColumn
            [ scroller
                [ styled S.button [ margin <| rem 0.5 ] [ E.onClick ClickedMsg ] [ S.text "Click" ]
                ]
            ]
        ]


leftColumn children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 8
        , alignItems stretch
        , justifyContent stretch
        ]
        []
        children


rightColumn children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 1
        , alignItems stretch
        , justifyContent stretch
        , borderStyle solid
        , borderWidth4 (px 0) (px 0) (px 0) (px 1)
        , borderColor <| theme.line
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


type alias NodeContent =
    { ctx : String, term : String, ty : String }


type Tree
    = Node NodeContent (List Tree)


exampleContent =
    { ctx = "TypeVar1, termVar1: TypeVar1, TypeVar2, termVar2: TypeVar1"
    , term = "(λ x: TypeVar1. x) termVar2"
    , ty = "TypeVar1"
    }


tree =
    Node exampleContent [ Node exampleContent [], Node exampleContent [] ]


drawTree t =
    case t of
        Node content children ->
            styled S.div
                [ margin2 (rem 0) (rem 2) ]
                []
                [ styled S.div
                    [ displayFlex, margin2 (rem 0) (rem -2) ]
                    []
                    (children
                        |> List.map drawTree
                    )
                , hairline
                , proofCell content
                ]


hairline =
    styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.line, width <| pct 100 ] [] []
