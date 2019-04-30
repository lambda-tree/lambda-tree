module RuleTree.View.ProofCell exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Lambda.ParseTransform exposing (ParseTransformError(..))
import Lambda.Rule exposing (RuleError(..))
import Maybe.Extra
import RuleTree.Model exposing (TextKind(..))
import RuleTree.ViewModel exposing (TreeViewDataResult)
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)


showRuleError : RuleError -> String
showRuleError ruleError =
    case ruleError of
        ParseError { col } ->
            "Error parsing expression at " ++ String.fromInt col

        ParseTransformError (IndexNotFound str) ->
            "Variable '" ++ str ++ "' is not in context"

        PrerequisiteDataError ->
            "Error in context"


getTitle : Maybe RuleError -> List (S.Attribute msg)
getTitle =
    Maybe.map showRuleError
        >> Maybe.map A.title
        >> Maybe.Extra.toList


proofCell : { a | ctx : TreeViewDataResult, term : TreeViewDataResult, ty : TreeViewDataResult } -> (TextKind -> String -> msg) -> S.Html msg
proofCell content msgCreator =
    S.div
        []
        [ styled S.div
            [ displayFlex, flexShrink <| int 0, alignItems center, margin <| rem 0.5, justifyContent center, minHeight <| px 32 ]
            []
            [ styled S.div
                [ displayFlex
                , width <| px 170
                ]
                (getTitle content.ctx.error)
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
                (getTitle content.term.error)
                [ lambdaExprInput
                    [ View.Lambda.ExpressionInput.Value content.term.text
                    , View.Lambda.ExpressionInput.OnInput (msgCreator TermKind)
                    , View.Lambda.ExpressionInput.Error content.term.error
                    ]
                ]
            , lambdaExprText ":"
            , styled S.div
                [ displayFlex
                , width <| px 170
                ]
                (getTitle content.ty.error)
                [ lambdaExprInput
                    [ View.Lambda.ExpressionInput.Value content.ty.text
                    , View.Lambda.ExpressionInput.OnInput (msgCreator TyKind)
                    , View.Lambda.ExpressionInput.Error content.ty.error
                    ]
                ]
            ]
        ]
