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
            [ displayFlex, flexShrink <| int 0, alignItems center, minWidth <| rem 45, margin <| rem 0.5 ]
            []
            [ styled S.div
                [ displayFlex
                , width <| px 170
                , backgroundColor <| getBackgroundColor content.ctx
                ]
                []
                [ lambdaExprInput True content.ctx.text (msgCreator CtxKind) ]
            , lambdaExprText "⊢"
            , styled S.div
                [ displayFlex
                , width <| px 340
                , backgroundColor <| getBackgroundColor content.term
                ]
                []
                [ lambdaExprInput False content.term.text (msgCreator TermKind) ]
            , lambdaExprText ":"
            , styled S.div
                [ displayFlex
                , width <| px 170
                , backgroundColor <| getBackgroundColor content.ty
                ]
                []
                [ lambdaExprInput False content.ty.text (msgCreator TyKind) ]
            ]
        ]
