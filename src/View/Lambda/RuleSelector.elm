module View.Lambda.RuleSelector exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Events as E
import Lambda.Rule exposing (Rule(..))
import RuleTree.Message exposing (Msg(..))
import View.Theme exposing (theme)


ruleSelector createMsgClick =
    styled S.div
        []
        []
        (List.map (ruleButton createMsgClick) [ TTrue, TFalse, TVar, TVarInst, TAbs, TApp, TIf, TTAbs, TTApp, TLet, TLetGen, TGen, TInst ])


ruleButton createClick r =
    styled S.button [ theme.font ] [ E.onClick <| createClick r ] [ S.text <| Debug.toString r ]


ruleDropDown button path dropdownState =
    Dropdown.dropdown
        dropdownState
        { options = []
        , toggleMsg = RuleDropdownMsg path
        , toggleButton = button
        , items =
            [ Dropdown.buttonItem [ HtmlE.onClick <| RuleSelectedMsg path NoRule ] [ Html.text "None" ]
            , Dropdown.buttonItem [ HtmlE.onClick <| RuleSelectedMsg path NoRule, HtmlA.disabled True ] [ Html.text "Hint" ]
            , Dropdown.divider
            , Dropdown.buttonItem [ HtmlE.onClick <| RuleSelectedMsg path TTAbs ] [ Html.text "T-TAbs" ]
            , Dropdown.buttonItem [ HtmlE.onClick <| RuleSelectedMsg path TIf ] [ Html.text "T-If" ]
            ]
        }
        |> S.fromUnstyled


selectedRuleDDButton rule =
    Dropdown.toggle
        [ Button.small
        , Button.outlineDark
        , Button.attrs [ HtmlA.style "borderWidth" "0", HtmlA.style "outline" "none" ]
        ]
        [ S.text (Debug.toString rule) |> S.toUnstyled ]


newRuleDDButton =
    Dropdown.toggle
        [ Button.small
        , Button.light
        , Button.attrs [ HtmlA.class "removecaret" ]
        ]
        [ S.text "+" |> S.toUnstyled ]
