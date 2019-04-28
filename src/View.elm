module View exposing (..)

import Bootstrap.Button as Button
import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Lambda.Parse exposing (symbols)
import Message exposing (Msg(..))
import Model
import RuleTree.Message exposing (Msg(..))
import RuleTree.View.Tree exposing (drawTree)
import Settings.Message exposing (Msg(..))
import Settings.Model exposing (TypeSystem(..))
import Substitutor.Message
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)
import View.Lambda.RuleList exposing (ruleList)
import View.SegmentedControl exposing (segmentedControl)
import View.Switch as Switch
import View.Theme exposing (theme)


view : Model.Model -> Html Message.Msg
view model =
    styled S.div
        [ backgroundColor theme.background
        , height <| vh 100
        , displayFlex
        , overflow hidden
        , justifyContent stretch
        ]
        []
        [ leftColumn model
        , rightColumn model
        ]


leftColumn model =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex auto
        , justifyContent stretch
        , overflow hidden
        ]
        []
        [ topBar model
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


topBar model =
    appBar
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
            , segmentedControl [ View.SegmentedControl.SelectedIdx model.settings.typeSystem ]
                [ View.SegmentedControl.SegmentedControlItem
                    Settings.Model.SimplyTyped
                    (SettingsMsg << TypeSystemChangedMsg)
                    { text = Just symbols.lambda, sup = Just symbols.arrow, image = Nothing }
                , View.SegmentedControl.SegmentedControlItem
                    Settings.Model.HM
                    (SettingsMsg << TypeSystemChangedMsg)
                    { text = Just "H-M", image = Nothing, sup = Nothing }
                , View.SegmentedControl.SegmentedControlItem
                    Settings.Model.SystemF
                    (SettingsMsg << TypeSystemChangedMsg)
                    { text = Just "System F", image = Nothing, sup = Nothing }
                ]
            , styled S.div [ flex <| int 1 ] [] []
            , checkSwitch model
            , styled S.div [ width <| px 20 ] [] []
            , Button.button [ Button.small, Button.dark ] [ S.text "Export" |> S.toUnstyled ]
                |> S.fromUnstyled
            , styled S.div [ width <| px 10 ] [] []
            , Button.button [ Button.small, Button.dark, Button.onClick (HintMsg |> RuleTreeMsg) ] [ S.text "Hint" |> S.toUnstyled ]
                |> S.fromUnstyled
            ]
        ]


ruleBar model =
    appBar
        [ styled
            S.div
            [ displayFlex
            , flex <| auto
            , alignItems center
            , justifyContent stretch
            , padding2 (px 10) <| px 15
            ]
            []
            [ styled S.span
                [ theme.font, color theme.textOnDark, fontSize <| rem 1.3 ]
                []
                [ S.text
                    "Rules"
                ]
            , styled S.div [ flex <| int 1 ] [] []
            , styled S.div [ flex <| int 1 ] [] []
            , styled S.div [ flex <| int 1 ] [] []
            , combinedRulesSwitch model
            ]
        ]


appBar children =
    styled S.div
        [ height <| px 48
        , backgroundColor <| hex "#263238"
        , displayFlex
        , justifyContent stretch
        ]
        []
        children


rightColumn model =
    styled S.div
        [ displayFlex
        , flexDirection column
        , width <| px 250
        , minWidth <| px 250
        , justifyContent stretch
        , overflow hidden
        , borderWidth4 (px 0) (px 0) (px 0) (px 1)
        , borderStyle solid
        , marginLeft <| px -1
        , borderColor <| theme.line
        ]
        []
        [ ruleBar model
        , styled S.div
            [ displayFlex
            , flexDirection column
            , alignItems stretch
            , justifyContent flexStart
            , overflow auto
            , whiteSpace noWrap
            ]
            []
            [ ruleListContainer model
            ]
        ]


ruleListContainer model =
    styled S.div
        [ displayFlex
        , flexDirection column
        , justifyContent flexStart
        , alignItems stretch
        , overflow scroll
        ]
        []
        [ S.text
            "Substitute free variable"
        , lambdaExprInput False model.substitution.ty (Substitutor.Message.TyChangedMsg >> SubstitutionMsg)
        , lambdaExprText "/"
        , lambdaExprInput False model.substitution.var (Substitutor.Message.VarChangedMsg >> SubstitutionMsg)
        , styled S.button [ margin <| rem 0.5 ] [ E.onClick (DoSubstitutionMsg model.substitution |> RuleTreeMsg) ] [ S.text "Substitute" ]
        , ruleList <| (RuleClickedMsg >> RuleTreeMsg)
        ]


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


checkSwitch model =
    styled S.div
        [ displayFlex, alignItems center ]
        []
        [ styled S.span [ marginRight <| px 10, color theme.secondaryOnDark, theme.font ] [] [ S.text "Check" ]
        , Switch.switch model.settings.checkErrors (SettingsMsg << Settings.Message.CheckErrorsChangedMsg)
        ]


combinedRulesSwitch model =
    styled S.div
        [ displayFlex, alignItems center ]
        []
        [ styled S.span [ marginRight <| px 10, color theme.secondaryOnDark, theme.font ] [] [ S.text "Combined" ]
        , Switch.switch model.settings.useCombinedRules (SettingsMsg << Settings.Message.UseCombinedRulesChangedMsg)
        ]
