module RuleTree.View.RuleDropdown exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Css exposing (..)
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Events as E
import Lambda.Rule exposing (Rule(..))
import Lambda.Show exposing (showRule)
import Message exposing (Msg(..))
import RuleTree.Message exposing (Msg(..))
import Substitutor.Message
import Substitutor.Model
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)
import View.Theme exposing (theme)


smallCapsClass =
    HtmlA.class "small-caps"


substitutor : List Int -> Substitutor.Model.Model -> S.Html RuleTree.Message.Msg
substitutor path substitution =
    S.div []
        [ S.text
            "Substitute free variable"
        , lambdaExprInput
            [ View.Lambda.ExpressionInput.Value substitution.ty
            , View.Lambda.ExpressionInput.OnInput (Substitutor.Message.TyChangedMsg >> SubstitutionMsg path)
            ]
        , lambdaExprText "/"
        , lambdaExprInput [ View.Lambda.ExpressionInput.Value substitution.var, View.Lambda.ExpressionInput.OnInput (Substitutor.Message.VarChangedMsg >> SubstitutionMsg path) ]
        , styled S.button [ margin <| rem 0.5 ] [ E.onClick (DoSubstitutionMsg substitution) ] [ S.text "Substitute" ]
        ]


ruleDropdown dropdownState { substitution, button, path, rules } =
    Dropdown.dropdown
        dropdownState
        { options = []
        , toggleMsg = RuleDropdownMsg path
        , toggleButton = button
        , items =
            [ Dropdown.buttonItem [ HtmlE.onClick <| RuleSelectedMsg path NoRule, HtmlA.disabled True ] [ Html.text "Hint Rule Selection" ]
            , Dropdown.buttonItem [ HtmlE.onClick <| RuleSelectedMsg path NoRule, HtmlA.disabled True ] [ Html.text "Hint Rule Premises" ]
            , Dropdown.buttonItem [ HtmlE.onClick <| RuleSelectedMsg path NoRule, HtmlA.disabled True ] [ Html.text "Autocomplete Tree" ]
            , Dropdown.customItem (substitutor path substitution |> S.toUnstyled)
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
