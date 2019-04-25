module View.Layout exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, fromUnstyled, styled, toUnstyled)
import Html.Styled.Events as E
import Material.Button as Button
import Material.Options as Options
import Message exposing (Msg(..))
import Model
import RuleTree.Message exposing (Msg(..))
import RuleTree.View.Tree exposing (drawTree)
import Substitutor.Message
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)
import View.Lambda.RuleList exposing (ruleList)
import View.Theme exposing (theme)


mainContent : Model.Model -> Html Message.Msg
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
        [ leftColumn
            [ treeContainer
                [ drawTree model.ruleTree |> S.map RuleTreeMsg
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
                [ S.button [ E.onClick (HintMsg |> RuleTreeMsg) ] [ S.text "Hint" ]

                --                , S.text
                --                    "Substitute free variable"
                --                , lambdaExprInput False model.substitution.ty (Substitutor.Message.TyChanged >> SubstitutionMsg)
                --                , lambdaExprText "/"
                --                , lambdaExprInput False model.substitution.var (Substitutor.Message.VarChanged >> SubstitutionMsg)
                --                , styled S.button [ margin <| rem 0.5 ] [ E.onClick (DoSubstitutionMsg model.substitution |> RuleTreeMsg) ] [ S.text "Substitute" ]
                , Button.view Mdc
                    "my-button"
                    model.mdc
                    [ Button.ripple
                    , Options.onClick (HintMsg |> RuleTreeMsg)
                    ]
                    [ S.text "Click me!" |> toUnstyled ]
                    |> fromUnstyled
                ]

            --            , ruleList <| (RuleClickedMsg >> RuleTreeMsg)
            ]
        ]


leftColumn children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 8
        , alignItems flexStart
        , justifyContent flexStart
        , overflow auto
        , whiteSpace noWrap
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
