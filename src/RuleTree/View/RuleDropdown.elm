module RuleTree.View.RuleDropdown exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Html.Styled as S
import Lambda.Rule exposing (Rule(..), showRule)
import RuleTree.Message exposing (Msg(..))


smallCapsClass =
    HtmlA.class "small-caps"


ruleDropdown dropdownState { button, path, rules, hintDisabled } =
    Dropdown.dropdown
        dropdownState
        { options = []
        , toggleMsg = RuleDropdownMsg path
        , toggleButton = button
        , items =
            [ Dropdown.buttonItem
                [ HtmlE.onClick <| HintPremisesMsg path
                , HtmlA.disabled hintDisabled
                , HtmlA.title <|
                    if hintDisabled then
                        "Error in term"

                    else
                        "Automatically fill in all the premises"
                ]
                [ Html.text "Autocomplete Premises" ]
            , Dropdown.buttonItem
                [ HtmlE.onClick <| HintBranchMsg path
                , HtmlA.disabled hintDisabled
                , HtmlA.title <|
                    if hintDisabled then
                        "Error in term"

                    else
                        "Automatically complete the whole branch of the derivation tree"
                ]
                [ Html.text "Autocomplete Branch" ]
            , Dropdown.buttonItem
                [ HtmlE.onClick <| HintRuleSelectionMsg path
                , HtmlA.disabled hintDisabled
                , HtmlA.title <|
                    if hintDisabled then
                        "Error in term"

                    else
                        "Automatically select the correct rule"
                ]
                [ Html.text "Hint Rule Selection" ]
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
