module RuleTree.View.RuleDropdown exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Html.Styled as S
import Lambda.Rule exposing (Rule(..))
import Lambda.Show.Text exposing (showRule)
import RuleTree.Message exposing (Msg(..))


smallCapsClass =
    HtmlA.class "small-caps"


ruleDropdown dropdownState { button, path, rules } =
    Dropdown.dropdown
        dropdownState
        { options = []
        , toggleMsg = RuleDropdownMsg path
        , toggleButton = button
        , items =
            [ Dropdown.buttonItem [ HtmlE.onClick <| HintRuleSelectionMsg path, HtmlA.disabled False ] [ Html.text "Hint Rule Selection" ]
            , Dropdown.buttonItem [ HtmlE.onClick <| RuleSelectedMsg path NoRule, HtmlA.disabled True ] [ Html.text "Hint Rule Premises" ]
            , Dropdown.buttonItem [ HtmlE.onClick <| HintTreeMsg path, HtmlA.disabled False ] [ Html.text "Autocomplete Tree" ]
            , Dropdown.divider
            , Dropdown.buttonItem [ HtmlE.onClick <| RuleSelectedMsg path NoRule, smallCapsClass ] [ Html.text "No Rule" ]
            ]
                ++ List.map (\rule -> Dropdown.buttonItem [ HtmlE.onClick <| RuleSelectedMsg path rule, smallCapsClass ] [ Html.text <| showRule rule ]) rules
        }
        |> S.fromUnstyled


selectedRuleDDButton rule =
    Dropdown.toggle
        [ Button.small
        , Button.outlineDark
        , Button.attrs [ HtmlA.class "borderless-dropdown", smallCapsClass ]
        ]
        [ Html.text <| showRule rule ++ " " ]


newRuleDDButton =
    Dropdown.toggle
        [ Button.small
        , Button.light
        , Button.attrs [ HtmlA.class "removecaret" ]
        ]
        [ Html.text "+" ]
