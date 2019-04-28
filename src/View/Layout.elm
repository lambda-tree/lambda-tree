module View.Layout exposing (..)

import Css exposing (..)
import Dict
import Html
import Html.Attributes
import Html.Styled as S exposing (Html, fromUnstyled, styled, toUnstyled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg(..))
import Model
import RuleTree.Message exposing (Msg(..))
import RuleTree.View.Tree exposing (drawTree)
import Substitutor.Message
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)
import View.Lambda.RuleList exposing (ruleList)
import View.Switch as Switch
import View.Theme exposing (theme)


tab : Message.TypeSystem -> Html Message.Msg
tab typeSystem =
    S.div
        []
        [ S.text
            (case typeSystem of
                Message.SimplyTyped ->
                    "λ->"

                Message.HM ->
                    "H-M"

                Message.SystemF ->
                    "System F"
            )
        ]


mainContent : Model.Model -> Html Message.Msg
mainContent model =
    styled S.div
        [ displayFlex
        , flex <| auto
        , alignItems <| stretch
        , justifyContent stretch
        , height <| pct 100
        , width <| pct 100
        , maxWidth <| pct 100
        ]
        []
        [ styled S.div
            [ displayFlex
            , flexDirection column
            , flex <| int 1
            , alignItems flexStart
            , justifyContent flexStart
            , height <| pct 100
            , minHeight <| pct 100
            ]
            []
            [ styled S.div
                [ height <| px 48
                , backgroundColor <| hex "#263238"
                , displayFlex
                , justifyContent stretch
                ]
                []
                [ styled
                    S.div
                    [ displayFlex
                    , flex <| auto
                    , alignItems center
                    , justifyContent stretch
                    , padding2 (px 10) <| px 15
                    ]
                    []
                    [ styled S.img
                        [ width auto, height <| px 20, marginRight <| px 20 ]
                        [ A.src "/img/logo.svg"
                        ]
                        []
                    , styled S.div [ flex <| int 1 ] [] []
                    , styled S.div [ flex <| int 1 ] [] []
                    , checkSwitch model ToggleChecking
                    ]
                ]
            , styled S.div
                [ displayFlex
                , flex <| int 1
                , flexDirection column
                , alignItems flexStart
                , justifyContent flexStart
                , overflow auto
                , whiteSpace noWrap
                ]
                []
                [ treeContainer
                    [ drawTree model.ruleTree |> S.map RuleTreeMsg
                    ]
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
                ]

            --            , ruleList <| (RuleClickedMsg >> RuleTreeMsg)
            ]
        ]


checkSwitch model onClick =
    styled S.div
        [ displayFlex, alignItems center ]
        []
        [ styled S.span [ marginRight <| px 10, color theme.textOnDark, theme.cmFont ] [] [ S.text "Check" ]
        , Switch.switch model.showErrors onClick
        , styled S.span [ marginRight <| px 30 ] [] []
        ]



--
--mainContent : Model.Model -> Html Message.Msg
--mainContent model =
--    styled S.div
--        [ displayFlex
--        , height <| vh 100
--        ]
--        []
--        [ Html.div []
--            [ top model |> toUnstyled
--            ]
--            |> fromUnstyled
--        , styled S.div
--            [ width <| px 200 ]
--            []
--            []
--        ]


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
        , width <| px 200
        , minWidth <| px 200
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
