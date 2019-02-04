module View.Lambda.ProofCell exposing (..)

import Html.Styled as S exposing (Html, styled)
import Css exposing (..)
import Message exposing (Msg)
import Model exposing (TextKind(..))
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)


proofCell : { a | ctx : String, term : String, ty : String } -> (TextKind -> String -> Msg) -> S.Html Msg
proofCell content msgCreator =
    S.div
        []
        [ styled S.div
            [ displayFlex, flexShrink <| int 0, alignItems center, minWidth <| rem 45, margin <| rem 0.5 ]
            []
            [ styled S.div [ displayFlex, flex <| int 1 ] [] [ lambdaExprInput True content.ctx (msgCreator CtxKind) ]
            , lambdaExprText "⊢"
            , styled S.div [ displayFlex, flex <| int 2 ] [] [ lambdaExprInput False content.term (msgCreator TermKind) ]
            , lambdaExprText ":"
            , styled S.div [ displayFlex, flex <| int 1 ] [] [ lambdaExprInput False content.ty (msgCreator TyKind) ]
            ]
        ]
