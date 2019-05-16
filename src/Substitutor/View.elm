module Substitutor.View exposing (..)

import Bootstrap.Button as Button
import Css exposing (..)
import Html
import Html.Events as HtmlE
import Html.Styled as S exposing (Html, styled)
import Lambda.Rule exposing (ExprError(..))
import Message exposing (Msg(..))
import Substitutor.Message
import Substitutor.Model
import Substitutor.Utils exposing (isValid, parsedType, parsedVar)
import Utils.Utils exposing (extractError)
import View.ExpressionInput exposing (lambdaExprInput)
import View.Theme exposing (theme)


spacer length =
    styled S.div [ width <| px length ] [] []


txt str =
    styled S.span [ fontSize <| rem 0.8125, color <| theme.secondary ] [] [ S.text str ]


view substitution =
    styled S.div
        [ displayFlex
        , alignItems center
        , justifyContent stretch
        , flexShrink <| int 1
        ]
        []
        [ S.span []
            [ txt "Substitute"
            , S.br [] []
            , txt "Type"
            ]
        , spacer 15
        , lambdaExprInput
            [ View.ExpressionInput.Value substitution.ty
            , View.ExpressionInput.Placeholder "e.g.   X2 → Bool"
            , View.ExpressionInput.Error (parsedType substitution |> extractError |> Maybe.map ParseError)
            , View.ExpressionInput.OnInput (SubstitutionMsg << Substitutor.Message.TyChangedMsg)
            , View.ExpressionInput.ExpandLambdaShortcuts True
            , View.ExpressionInput.Size 14
            ]
        , spacer 20
        , S.span []
            [ txt "For Type"
            , S.br [] []
            , txt "Variable"
            ]
        , spacer 15
        , lambdaExprInput
            ([ View.ExpressionInput.Value substitution.var
             , View.ExpressionInput.Placeholder "e.g.   X1"
             , View.ExpressionInput.Error (parsedVar substitution |> extractError |> Maybe.map ParseError)
             , View.ExpressionInput.OnInput (SubstitutionMsg << Substitutor.Message.VarChangedMsg)
             , View.ExpressionInput.Size 6
             ]
                ++ (if isValid substitution then
                        [ View.ExpressionInput.OnEnter (\_ -> DoSubstitutionMsg) ]

                    else
                        []
                   )
            )
        , spacer 20
        , Button.button
            [ Button.small
            , Button.dark
            , Button.disabled (isValid substitution |> not)
            , Button.onClick DoSubstitutionMsg
            , Button.attrs []
            ]
            [ S.text "Substitute" |> S.toUnstyled
            ]
            |> S.fromUnstyled
        ]
