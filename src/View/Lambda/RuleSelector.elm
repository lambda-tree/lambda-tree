module View.Lambda.RuleSelector exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Model exposing (Rule(..))
import View.Theme exposing (theme)


ruleSelector createMsgClick =
    styled S.div
        []
        []
        (List.map (ruleButton createMsgClick) [ TTrue, TFalse, TVar, TAbs, TApp, TIf, TTAbs, TTApp, TLet, TGen, TInst ])


ruleButton createClick r =
    styled S.button [ theme.font ] [ E.onClick <| createClick r ] [ S.text <| Debug.toString r ]
