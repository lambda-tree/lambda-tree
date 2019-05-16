module View.ExpressionInput exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Lambda.Rule exposing (ExprError)
import Maybe.Extra
import Utils.Utils exposing (onKeydownEnter)
import View.Theme exposing (theme)


type LambdaExprInputOption msg
    = Value String
    | OnInput (String -> msg)
    | OnEnter (String -> msg)
    | Error (Maybe ExprError)
    | Placeholder String
    | ExpandLambdaShortcuts Bool
    | Size Int


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

        valueStrings =
            options
                |> List.filterMap
                    (\x ->
                        case x of
                            Value "" ->
                                Nothing

                            Value str ->
                                Just <| str

                            _ ->
                                Nothing
                    )

        value =
            valueStrings |> List.map A.value

        onEnter =
            options
                |> List.filterMap
                    (\x ->
                        case x of
                            OnEnter msg ->
                                Just <|
                                    A.fromUnstyled <|
                                        onKeydownEnter
                                            (msg (valueStrings |> List.head |> Maybe.withDefault ""))

                            _ ->
                                Nothing
                    )

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

        placeholderStrings =
            options
                |> List.filterMap
                    (\x ->
                        case x of
                            Placeholder s ->
                                Just <| s

                            _ ->
                                Nothing
                    )

        placeholder =
            List.map A.placeholder placeholderStrings

        replaceSpecialCharsClass =
            options
                |> List.filterMap
                    (\x ->
                        case x of
                            ExpandLambdaShortcuts True ->
                                Just <| A.class "expand-lambda-shortcuts"

                            _ ->
                                Nothing
                    )

        size =
            options
                |> List.filterMap
                    (\x ->
                        case x of
                            Size i ->
                                Just <| i

                            _ ->
                                Nothing
                    )
                |> List.head
                |> Maybe.withDefault 1

        paddingHorizontal =
            px 10

        commonStyles =
            batch
                [ fontSize <| rem 1
                , theme.font
                , borderWidth <| px 0
                , border <| rem 0
                , property "-webkit-appearance" "none"
                , property "-moz-appearance" "none"
                , property "-ms-appearance" "none"
                , property "-o-appearance" "none"
                , property "appearance" "none"
                , outline none
                , overflow hidden
                ]
    in
    styled S.div
        [ displayFlex, flexDirection column, flexShrink <| int 1, flexGrow <| int 0 ]
        []
        [ styled S.input
            [ commonStyles
            , flex auto
            , color theme.text
            , backgroundColor theme.inputBackground
            , boxShadow4 (px 0)
                (px 0)
                (if Maybe.Extra.isJust error then
                    px 3

                 else
                    px 0
                )
                (rgba 255 0 0 0.5)
            , padding2 (rem 0.3) paddingHorizontal
            , pseudoElement "placeholder"
                [ opacity <| num 0.9
                ]
            , borderRadius <| rem 0.5
            ]
            (value
                ++ onInput
                ++ onEnter
                ++ placeholder
                ++ replaceSpecialCharsClass
                ++ [ A.autocomplete False, A.spellcheck False, A.size size ]
            )
            []
        , styled S.span
            [ commonStyles
            , visibility hidden
            , height <| px 0
            , padding2 (px 0) paddingHorizontal
            , marginRight <| px 3
            , whiteSpace noWrap
            , flexBasis <| auto
            , flexGrow <| int 0
            , flexShrink <| int 1
            , overflow hidden
            ]
            []
            [ S.text <|
                String.replace " "
                    "\u{00A0}"
                    (List.head valueStrings
                        |> Maybe.withDefault
                            (List.head placeholderStrings
                                |> Maybe.withDefault ""
                            )
                    )
            ]
        ]
