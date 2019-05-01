module View exposing (..)

import Bootstrap.Button as Button
import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Lambda.Parse exposing (symbols)
import Lambda.Rule exposing (rulesForTypeSystem)
import Message exposing (Msg(..))
import Model
import RuleTree.Message exposing (Msg(..))
import RuleTree.View.Tree exposing (treeView)
import Settings.Message exposing (Msg(..))
import Settings.Model exposing (TypeSystem(..))
import Settings.Utils exposing (getTypeSystem)
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
            , flex <| auto
            , flexDirection column
            , alignItems flexStart
            , justifyContent flexStart
            , overflow auto
            , whiteSpace noWrap
            ]
            []
            [ treeContainer model ]
        ]


topBar : Model.Model -> S.Html Message.Msg
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
                    SimplyTyped
                    (SettingsMsg << TypeSystemChangedMsg)
                    { text = Just symbols.lambda, sup = Just symbols.arrow, image = Nothing }
                , View.SegmentedControl.SegmentedControlItem
                    HM
                    (SettingsMsg << TypeSystemChangedMsg)
                    { text = Just "H–M", image = Nothing, sup = Nothing }
                , View.SegmentedControl.SegmentedControlItem
                    SystemF
                    (SettingsMsg << TypeSystemChangedMsg)
                    { text = Just "System F", image = Nothing, sup = Nothing }
                ]
            , styled S.div [ width <| px 20 ] [] []
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
            ([ styled S.span
                [ theme.font, color theme.textOnDark, fontSize <| rem 1.3 ]
                []
                [ S.text
                    "Rules"
                ]
             , styled S.div [ flex <| int 1 ] [] []
             ]
                ++ (if model.settings.typeSystem == HM then
                        [ combinedRulesSwitch model ]

                    else
                        []
                   )
            )
        ]


appBar children =
    styled S.div
        [ height <| px 48
        , minHeight <| px 48
        , backgroundColor <| theme.backgroundDark
        , displayFlex
        , justifyContent stretch
        , overflow auto
        ]
        []
        children


rightColumn model =
    styled S.div
        [ displayFlex
        , flexDirection column
        , width <| px 350
        , minWidth <| px 350
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
        , backgroundColor theme.inputBackground
        , justifyContent flexStart
        , alignItems stretch
        , overflow scroll
        ]
        []
        [ ruleList (getTypeSystem model.settings |> rulesForTypeSystem)
        ]


treeContainer model =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems center
        , padding <| px 20
        , marginLeft auto
        , marginRight auto
        ]
        []
        [ treeView model.settings model.ruleTree |> S.map RuleTreeMsg
        ]


checkSwitch model =
    styled S.div
        [ displayFlex, alignItems center ]
        [ A.title "Show errors in derivation tree" ]
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
