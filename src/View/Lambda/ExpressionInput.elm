module View.Lambda.ExpressionInput exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Lambda.Rule exposing (ExprError)
import Maybe.Extra
import View.Theme exposing (theme)


type LambdaExprInputOption msg
    = Value String
    | OnInput (String -> msg)
    | Error (Maybe ExprError)
    | AlignRight


lambdaExprInput : List (LambdaExprInputOption msg) -> Html msg
lambdaExprInput options =
    let
        onInput =
            options
                |> List.filterMap
                    (\x ->
                        case x of
                            OnInput msg ->
                                Just <| E.onInput msg

                            _ ->
                                Nothing
                    )

        value =
            options
                |> List.filterMap
                    (\x ->
                        case x of
                            Value str ->
                                Just <| A.value str

                            _ ->
                                Nothing
                    )

        alignRight =
            options |> List.any ((==) AlignRight)

        error =
            options
                |> List.filterMap
                    (\x ->
                        case x of
                            Error (Just e) ->
                                Just <| e

                            _ ->
                                Nothing
                    )
                |> List.head
    in
    styled S.input
        [ color theme.text
        , backgroundColor theme.inputBackground
        , boxShadow4 (px 0)
            (px 0)
            (if Maybe.Extra.isJust error then
                px 3

             else
                px 0
            )
            (rgba 255 0 0 0.5)
        , fontSize <| rem 1
        , theme.font
        , width <| pct 100
        , padding2 (rem 0.3) (rem 0.6)
        , borderRadius <| rem 0.5
        , borderWidth <| px 0
        , border <| rem 0
        , property "-webkit-appearance" "none"
        , property "-moz-appearance" "none"
        , property "-ms-appearance" "none"
        , property "-o-appearance" "none"
        , property "appearance" "none"
        , outline none
        , overflow hidden
        , property "direction" <|
            if alignRight then
                "rtl"

            else
                "ltr"
        ]
        (value ++ onInput ++ [ A.autocomplete False, A.spellcheck False ])
        []
