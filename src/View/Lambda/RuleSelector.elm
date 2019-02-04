module View.Lambda.RuleSelector exposing (..)

import Html.Styled.Events as E
import Html.Styled as S exposing (Html, styled)
import Model exposing (Rule(..))
import View.Theme exposing (theme)


ruleSelector createMsgClick =
    styled S.div
        []
        []
        (List.map (ruleButton createMsgClick) [ TTrue, TFalse, TVar, TAbs, TApp, TIf ])


ruleButton createClick r =
    styled S.button [ theme.font ] [ E.onClick <| createClick r ] [ S.text <| Debug.toString r ]
