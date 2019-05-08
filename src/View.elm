module View exposing (..)

import Bootstrap.Button as Button
import Css exposing (..)
import ErrorReport.View exposing (errorReportModal)
import Html.Attributes
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Lambda.Expression exposing (HMFlavor(..), TypeSystem(..))
import Lambda.Parse exposing (symbols)
import Lambda.Rule exposing (rulesForTypeSystem)
import Message exposing (Msg(..))
import Model
import RuleTree.View.Tree exposing (treeView)
import RuleTree.View.TreeToolbar exposing (treeToolbar)
import Settings.Message exposing (Msg(..))
import Settings.Utils exposing (getTypeSystem)
import Substitutor.View
import View.ExportDropdown exposing (exportDropdown)
import View.ImportButton exposing (importButton)
import View.RuleList exposing (ruleList)
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
        , if model.settings.showSidebar then
            rightColumn model

          else
            S.div [] []
        ]


leftColumn : Model.Model -> S.Html Message.Msg
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
            , flex auto
            , alignItems stretch
            , justifyContent stretch
            , overflow hidden
            , position relative
            ]
            []
            [ styled S.div
                [ displayFlex
                , flex auto
                , flexDirection column
                , alignItems flexStart
                , justifyContent flexStart
                , overflow auto
                , cursor grab
                ]
                [ A.class "dragscroll" ]
                [ treeContainer model
                , errorReportModal model.errorReport
                    |> S.map ErrorReportMsg
                ]
            , toolbar model
            , treeToolbarContainer model
            ]
        ]


toolbar : Model.Model -> S.Html Message.Msg
toolbar model =
    styled S.div
        [ position absolute
        , backgroundColor <| theme.clear
        , left <| px 20
        , right <| px 20
        , bottom <| px 0
        , displayFlex
        , justifyContent center
        ]
        []
        [ styled S.div
            [ displayFlex
            , justifyContent stretch
            , borderColor <| theme.darkLine
            , borderWidth4 (px 1) (px 1) (px 0) (px 1)
            , borderStyle solid
            , backgroundColor theme.background
            , padding2 (px 10) (px 15)
            , borderRadius4 (rem 0.2) (rem 0.2) (px 0) (px 0)
            , maxWidth <| pct 100
            ]
            []
            [ Substitutor.View.view model.substitution
            ]
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
                    { text = Just symbols.lambda
                    , sup = Just symbols.arrow
                    , image = Nothing
                    , info = Just "Simpy Typed Lambda Calculus"
                    }
                , View.SegmentedControl.SegmentedControlItem
                    (HM NonDeterministic)
                    (SettingsMsg << TypeSystemChangedMsg)
                    { text = Just "H–M"
                    , image = Nothing
                    , sup = Nothing
                    , info = Just "Hindley Milner (Original)"
                    }
                , View.SegmentedControl.SegmentedControlItem
                    (HM SyntaxDirected)
                    (SettingsMsg << TypeSystemChangedMsg)
                    { text = Just "H–M'"
                    , image = Nothing
                    , sup = Nothing
                    , info = Just "Hindley Milner (Syntax directed with combined rules)"
                    }
                , View.SegmentedControl.SegmentedControlItem
                    SystemF
                    (SettingsMsg << TypeSystemChangedMsg)
                    { text = Just "System F"
                    , image = Nothing
                    , sup = Nothing
                    , info = Just "System F"
                    }
                ]
            , styled S.div [ maxWidth <| px 50, minWidth <| px 10, flex auto ] [] []
            , exportDropdown model.exportDropdown
            , styled S.div [ width <| px 5 ] [] []
            , importButton
            , styled S.div [ maxWidth <| px 50, minWidth <| px 10, flex auto ] [] []
            , checkSwitch model
            , styled S.div [ width <| px 10 ] [] []
            , styled S.div [ flex <| int 1 ] [] []
            , Button.button
                [ Button.small
                , Button.outlineLight
                , Button.onClick (SettingsMsg <| Settings.Message.SidebarVisibilityChangedMsg (model.settings.showSidebar |> not))
                , Button.attrs [ Html.Attributes.class "borderless-dropdown" ]
                ]
                [ S.text
                    (if model.settings.showSidebar then
                        ">"

                     else
                        "< Rules"
                    )
                    |> S.toUnstyled
                ]
                |> S.fromUnstyled
            ]
        ]


ruleBar : S.Html msg
ruleBar =
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
            ]
        ]


appBar : List (S.Html msg) -> S.Html msg
appBar children =
    styled S.div
        [ height <| px 48
        , minHeight <| px 48
        , backgroundColor <| theme.backgroundDark
        , displayFlex
        , justifyContent stretch
        ]
        []
        children


rightColumn : Model.Model -> S.Html Message.Msg
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
        , borderColor <| theme.line
        , backgroundColor theme.inputBackground
        ]
        []
        [ ruleBar
        , ruleListContainer model
        ]


ruleListContainer : Model.Model -> S.Html msg
ruleListContainer model =
    styled S.div
        [ displayFlex
        , flexDirection column
        , justifyContent stretch
        , alignItems stretch
        , overflow auto
        ]
        []
        [ ruleList (getTypeSystem model.settings |> rulesForTypeSystem)
        ]


treeContainer : Model.Model -> S.Html Message.Msg
treeContainer model =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems center
        , padding4 (px 50) (px 20) (px 80) (px 20)
        , marginLeft auto
        , marginRight auto
        , position relative
        ]
        []
        [ treeView model.settings model.ruleTree |> S.map RuleTreeMsg
        ]


treeToolbarContainer : Model.Model -> S.Html Message.Msg
treeToolbarContainer model =
    styled S.div
        [ position absolute
        , backgroundColor <| theme.clear
        , right <| px 15
        , top <| px 15
        , displayFlex
        ]
        []
        [ treeToolbar model.ruleTree |> S.map RuleTreeMsg ]


checkSwitch : Model.Model -> S.Html Message.Msg
checkSwitch model =
    styled S.div
        [ displayFlex, alignItems center ]
        [ A.title "Show errors in derivation tree" ]
        [ styled S.span [ marginRight <| px 10, color theme.secondaryOnDark, theme.font ] [] [ S.text "Check" ]
        , Switch.switch (SettingsMsg << Settings.Message.CheckErrorsChangedMsg) model.settings.checkErrors
        ]
