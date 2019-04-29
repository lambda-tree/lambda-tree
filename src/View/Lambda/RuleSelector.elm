module View.Lambda.RuleSelector exposing (..)

import Bootstrap.Button as Button
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Events as E
import RuleTree.Model exposing (Rule(..))
import View.Theme exposing (theme)


ruleSelector createMsgClick =
    styled S.div
        []
        []
        (List.map (ruleButton createMsgClick) [ TTrue, TFalse, TVar, TVarInst, TAbs, TApp, TIf, TTAbs, TTApp, TLet, TLetGen, TGen, TInst ])


ruleButton createClick r =
    styled S.button [ theme.font ] [ E.onClick <| createClick r ] [ S.text <| Debug.toString r ]


rulePlus createMsgClick =
    Button.button [ Button.small, Button.light, Button.onClick createMsgClick ] [ S.text "+" |> S.toUnstyled ]
        |> S.fromUnstyled
