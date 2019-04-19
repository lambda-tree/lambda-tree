module View.Layout exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Events as E
import Message exposing (Msg(..))
import Substitutor.Message
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)
import View.Lambda.RuleList exposing (ruleList)
import View.Lambda.Tree exposing (drawTree)
import View.Theme exposing (theme)


mainContent model =
    styled S.div
        [ displayFlex
        , flex <| auto
        , alignItems <| stretch
        , justifyContent stretch
        , height <| pct 100
        , minHeight <| pct 100
        ]
        []
        [ leftColumn model.zoomLevel
            [ treeContainer
                [ drawTree model.tree
                ]
            ]
        , rightColumn
            [ styled S.div
                [ displayFlex
                , flexDirection column
                , justifyContent flexStart
                , alignItems stretch
                , overflow scroll
                ]
                []
                [ S.button [ E.onClick HintMsg ] [ S.text "Hint" ]
                , S.text
                    "Substitute free variable"
                , lambdaExprInput False model.substitution.ty (Substitutor.Message.TyChanged >> SubstitutionMsg)
                , lambdaExprText "/"
                , lambdaExprInput False model.substitution.var (Substitutor.Message.VarChanged >> SubstitutionMsg)
                , styled S.button [ margin <| rem 0.5 ] [ E.onClick DoSubstitutionMsg ] [ S.text "Substitute" ]
                ]
            , ruleList <| RuleClickedMsg
            ]
        ]


leftColumn zoomLevel children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 8
        , alignItems flexStart
        , justifyContent flexStart
        , overflow auto
        , whiteSpace noWrap
        , property "zoom" <| String.fromFloat zoomLevel ++ "%"
        ]
        []
        children


rightColumn children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 2
        , alignItems stretch
        , justifyContent flexStart
        , overflow auto
        , whiteSpace noWrap
        , borderStyle solid
        , borderWidth4 (px 0) (px 0) (px 0) (px 1)
        , borderColor <| theme.line
        ]
        []
        children


treeContainer children =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems center
        ]
        []
        children


scroller children =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems stretch
        , overflow scroll
        ]
        []
        children
