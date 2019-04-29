module RuleTree.View.ProofCell exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import RuleTree.Model exposing (TextKind(..))
import RuleTree.ViewModel exposing (TreeViewDataResult)
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)


getBackgroundColor : TreeViewDataResult -> Color
getBackgroundColor result =
    case result.error of
        Nothing ->
            rgba 0 0 0 0

        Just _ ->
            rgba 255 0 0 0.5


proofCell : { a | ctx : TreeViewDataResult, term : TreeViewDataResult, ty : TreeViewDataResult } -> (TextKind -> String -> msg) -> S.Html msg
proofCell content msgCreator =
    S.div
        []
        [ styled S.div
            [ displayFlex, flexShrink <| int 0, alignItems center, minWidth <| rem 45, margin <| rem 0.5, justifyContent center, minHeight <| px 32 ]
            []
            [ styled S.div
                [ displayFlex
                , width <| px 170
                ]
                []
                [ lambdaExprInput
                    [ View.Lambda.ExpressionInput.Value content.ctx.text
                    , View.Lambda.ExpressionInput.OnInput (msgCreator CtxKind)
                    , View.Lambda.ExpressionInput.Error content.ctx.error
                    ]
                ]
            , lambdaExprText "⊢"
            , styled S.div
                [ displayFlex
                , width <| px 340
                ]
                []
                [ lambdaExprInput [ View.Lambda.ExpressionInput.Value content.term.text, View.Lambda.ExpressionInput.OnInput (msgCreator TermKind), View.Lambda.ExpressionInput.Error content.term.error ] ]
            , lambdaExprText ":"
            , styled S.div
                [ displayFlex
                , width <| px 170
                ]
                []
                [ lambdaExprInput [ View.Lambda.ExpressionInput.Value content.ty.text, View.Lambda.ExpressionInput.OnInput (msgCreator TyKind), View.Lambda.ExpressionInput.Error content.ty.error ] ]
            ]
        ]
