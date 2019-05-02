module View.SubstitutionModal exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Css exposing (..)
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Lambda.Rule exposing (ExprError(..))
import Message exposing (Msg(..))
import Substitutor.Message
import Substitutor.Model
import Substitutor.Utils exposing (parsedType, parsedVar)
import Utils.Utils exposing (extractError)
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)
import View.Theme exposing (theme)


substitutor : Substitutor.Model.Model -> S.Html Message.Msg
substitutor substitution =
    styled S.div
        [ displayFlex, flex auto, flexDirection column ]
        []
        [ styled S.div [ marginBottom <| rem 0.5 ] [] [ S.text "Type" ]
        , lambdaExprInput
            [ View.Lambda.ExpressionInput.Value substitution.ty
            , View.Lambda.ExpressionInput.Placeholder "E.g. 'X2 → Bool'"
            , View.Lambda.ExpressionInput.Error (parsedType substitution |> extractError |> Maybe.map ParseError)
            , View.Lambda.ExpressionInput.OnInput (Substitutor.Message.TyChangedMsg >> SubstitutionMsg)
            ]
        , styled S.div [ margin2 (rem 0.5) (px 0) ] [] [ S.text "For Type Variable" ]
        , lambdaExprInput
            [ View.Lambda.ExpressionInput.Value substitution.var
            , View.Lambda.ExpressionInput.Placeholder "E.g. 'X1'"
            , View.Lambda.ExpressionInput.Error (parsedVar substitution |> extractError |> Maybe.map ParseError)
            , View.Lambda.ExpressionInput.OnInput (Substitutor.Message.VarChangedMsg >> SubstitutionMsg)
            , View.Lambda.ExpressionInput.OnEnter (\_ -> DoSubstitutionMsg)
            ]
        ]


substitutionModal modalState substitution =
    Modal.config HideSubstitutionModalMsg
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h5 [] [ Html.text "Substitute" ]
        |> Modal.body [] [ substitutor substitution |> S.toUnstyled ]
        |> Modal.footer []
            [ Button.button
                [ Button.dark
                , Button.attrs [ HtmlE.onClick DoSubstitutionMsg ]
                ]
                [ Html.text "Substitute" ]
            ]
        |> Modal.view modalState
