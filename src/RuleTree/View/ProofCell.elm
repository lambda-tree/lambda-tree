module RuleTree.View.ProofCell exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Lambda.ParseTransform exposing (ParseTransformError(..))
import Lambda.Rule exposing (ExprError(..))
import Maybe.Extra
import RuleTree.Model exposing (TextKind(..))
import RuleTree.ViewModel exposing (TreeViewDataResult)
import View.ExpressionInput exposing (lambdaExprInput)
import View.Theme exposing (theme)


showExprError : ExprError -> String
showExprError exprError =
    case exprError of
        ParseError { col } ->
            "Error parsing expression at " ++ String.fromInt col

        ParseTransformError (IndexNotFound str) ->
            "Variable '" ++ str ++ "' is not in context"

        PrerequisiteDataError ->
            "Error in context"

        NotInTypeSystemError ->
            "Expression is not valid in selected type system"

        EmptyTypeError ->
            "No type. Use type variable such as 'A1' to derive the type"


getTitle : Maybe ExprError -> List (S.Attribute msg)
getTitle =
    Maybe.map showExprError
        >> Maybe.map A.title
        >> Maybe.Extra.toList


proofCell : Bool -> Bool -> { a | ctx : TreeViewDataResult, term : TreeViewDataResult, ty : TreeViewDataResult } -> (TextKind -> String -> msg) -> S.Html msg
proofCell showPlaceholders checkErrors content msgCreator =
    styled S.div
        [ displayFlex, flexShrink <| int 0, alignItems center, margin <| rem 0.5, justifyContent center, minHeight <| px 32 ]
        []
        [ styled S.div
            [ displayFlex
            ]
            (getTitle content.ctx.error)
            [ lambdaExprInput <|
                [ View.ExpressionInput.Value content.ctx.text
                , View.ExpressionInput.OnInput (msgCreator CtxKind)
                , View.ExpressionInput.ExpandLambdaShortcuts True
                , View.ExpressionInput.Error <|
                    if checkErrors then
                        content.ctx.error

                    else
                        Nothing
                ]
                    ++ (if showPlaceholders then
                            [ View.ExpressionInput.Placeholder "e.g.   y: A, z: Bool→Bool"
                            ]

                        else
                            []
                       )
            ]
        , lambdaExprText "⊢"
        , styled S.div
            [ displayFlex
            ]
            (getTitle content.term.error)
            [ lambdaExprInput <|
                [ View.ExpressionInput.Value content.term.text
                , View.ExpressionInput.OnInput (msgCreator TermKind)
                , View.ExpressionInput.ExpandLambdaShortcuts True
                , View.ExpressionInput.Error <|
                    if checkErrors then
                        content.term.error

                    else
                        Nothing
                ]
                    ++ (if showPlaceholders then
                            [ View.ExpressionInput.Placeholder "e.g.   (λx: A. x) y"
                            ]

                        else
                            []
                       )
            ]
        , lambdaExprText ":"
        , styled S.div
            [ displayFlex
            ]
            (getTitle content.ty.error)
            [ lambdaExprInput <|
                [ View.ExpressionInput.Value content.ty.text
                , View.ExpressionInput.OnInput (msgCreator TyKind)
                , View.ExpressionInput.ExpandLambdaShortcuts True
                , View.ExpressionInput.Error <|
                    if checkErrors then
                        content.ty.error

                    else
                        Nothing
                ]
                    ++ (if showPlaceholders then
                            [ View.ExpressionInput.Placeholder "e.g.   Z"
                            ]

                        else
                            []
                       )
            ]
        ]


lambdaExprText value =
    styled S.span
        [ color theme.text
        , backgroundColor theme.clear
        , fontSize <| rem 1
        , theme.font
        , fontWeight normal
        , margin2 (rem 0) (rem 0.3)
        ]
        []
        [ S.text value ]
