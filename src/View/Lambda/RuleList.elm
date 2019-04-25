module View.Lambda.RuleList exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import RuleTree.Model exposing (Rule(..))
import View.Theme exposing (theme)


ruleListItem =
    S.text "Item 1"


ruleList createMsgClick =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems stretch
        , overflow scroll
        ]
        []
        (List.map
            (ruleButton createMsgClick)
            [ TTrue, TFalse, TVar, TAbs, TApp, TIf, TTAbs, TTApp, TLet, TGen, TInst ]
            ++ [ styled S.img [ width <| pct 100, height auto ] [ A.src "/img/if.png" ] []
               ]
        )


ruleButton createClick r =
    styled S.button [ theme.font ] [ E.onClick <| createClick r ] [ S.text <| Debug.toString r ]
