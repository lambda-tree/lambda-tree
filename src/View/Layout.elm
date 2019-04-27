module View.Layout exposing (..)

import Css exposing (..)
import Css.Global
import Dict
import Html
import Html.Attributes
import Html.Styled as S exposing (Html, fromUnstyled, styled, toUnstyled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Material
import Material.Button as Button
import Material.Drawer.Permanent as Drawer
import Material.Icon
import Material.Options as Options exposing (for, when)
import Material.Switch as Switch
import Material.TabBar as TabBar
import Material.Theme
import Material.TopAppBar as TopAppBar
import Material.Typography
import Message exposing (Msg(..))
import Model
import RuleTree.Message exposing (Msg(..))
import RuleTree.View.Tree exposing (drawTree)
import Substitutor.Message
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)
import View.Lambda.RuleList exposing (ruleList)
import View.Theme exposing (theme)


tab : Message.TypeSystem -> TabBar.Tab Message.Msg
tab typeSystem =
    TabBar.tab
        [ Options.onClick (SelectTypeSystemMsg typeSystem) ]
        [ Html.text
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
        , minHeight <| pct 100
        ]
        []
        [ styled S.div
            [ displayFlex
            , flexDirection column
            , flex <| int 8
            , alignItems stretch
            , justifyContent stretch
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
                    , S.div
                        []
                        [ let
                            index =
                                "tab-bar"

                            onDark =
                                Options.styled Html.div [ Material.Theme.textPrimaryOnDark ]
                          in
                          onDark
                            [ TabBar.view Mdc
                                index
                                model.mdc
                                [ TabBar.activeTab 0 ]
                                [ tab Message.SimplyTyped
                                , tab Message.HM
                                , tab Message.SystemF
                                ]
                            ]
                            |> fromUnstyled
                        ]
                    , styled S.div [ flex <| int 1 ] [] []
                    , checkSwitch model ToggleChecking |> fromUnstyled
                    , Button.view Mdc
                        "hint-button"
                        model.mdc
                        [ Button.ripple
                        , Button.raised
                        , Options.onClick (HintMsg |> RuleTreeMsg)
                        ]
                        [ S.text "Hint" |> toUnstyled ]
                        |> fromUnstyled
                    ]
                ]
            , styled S.div
                [ displayFlex
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


x : List (Html.Html msg) -> Html.Html msg
x =
    Options.styled Html.div
        [ Material.Typography.subtitle1, Material.Theme.textSecondaryOnDark ]


checkSwitch model onClick =
    x
        [ styled S.span [ marginRight <| px 10 ] [] [ S.text "Check" ] |> toUnstyled
        , Switch.view Mdc
            "check-switch"
            model.mdc
            [ Options.onClick onClick
            , Switch.on |> when model.showErrors
            ]
            []
        , styled S.span [ marginRight <| px 30 ] [] [] |> toUnstyled
        ]


top : Model.Model -> Html Message.Msg
top model =
    TopAppBar.view Mdc
        "top-bar"
        model.mdc
        [ TopAppBar.dense
        , Options.css "background-color" "#263238"
        ]
        [ TopAppBar.section
            [ TopAppBar.alignStart
            ]
            [ TopAppBar.title [] [ styled S.img [ width auto, height <| px 25 ] [ A.src "/img/logo.svg" ] [] |> toUnstyled ]
            ]
        , TopAppBar.section
            [ TopAppBar.alignEnd
            ]
            [ TopAppBar.actionItem [] "print"
            , TopAppBar.actionItem [] "bookmark"
            , checkSwitch model ToggleChecking
            ]
        ]
        |> fromUnstyled



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
