module Main exposing (..)

import Browser
import Debug exposing (log)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Css exposing (..)
import Css.Global exposing (global, body)
import Html.Styled as S exposing (Html, Attribute, styled, toUnstyled, Attribute, div)


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
    = EmptyMsg


update : Msg -> Model -> Model
update msg model =
    case msg of
        EmptyMsg ->
            {}



-- VIEW


theme : { background : Color, secondary : Color, text : Color }
theme =
    { background = rgb 240 240 240
    , text = hex "000000"
    , secondary = rgb 250 240 230
    }


background : List (Attribute msg) -> List (Html msg) -> Html msg
background =
    styled div
        [ backgroundColor theme.background
        , height <| px 100
        ]


lambdaExprInput value onInput =
    styled S.input
        [ color <| theme.text
        , fontSize <| rem 1
        , fontFamilies [ "Courier New" ]
        , width <| pct 100
        , marginLeft <| rem 1
        ]
        [ A.value value, E.onInput onInput ]
        []


lambdaExprText value =
    styled S.div
        [ color <| theme.text
        , fontSize <| rem 1
        , fontFamilies [ "Courier New" ]
        , marginLeft <| rem 1
        ]
        []
        [ S.button [] [ S.text value ] ]


impliesText =
    styled S.div [ fontFamilies [ "Arial" ], marginLeft <| rem 1 ] [] [ S.text " ⊢" ]


initialExprInput =
    lambdaExprInput "Λα. λf: α → α. λx: α. f (f x)" (\x -> log "MSG!" EmptyMsg)


initialCell =
    styled S.div
        [ displayFlex, alignItems center ]
        []
        [ lambdaExprText "Γ", impliesText, initialExprInput ]


sampleFormula =
    div [] []


view : Model -> Html Msg
view model =
    div []
        [ global [ body [ height <| pct 100, backgroundColor <| theme.background ] ]
        , initialCell
        ]
