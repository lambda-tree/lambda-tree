module RuleTree.View.TreeToolbar exposing (..)

import Bootstrap.Button as Button
import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import RuleTree.Message exposing (Msg(..))
import RuleTree.Model exposing (RuleTree)
import RuleTree.ViewModel exposing (isTreeEmpty)


treeToolbar : RuleTree -> S.Html RuleTree.Message.Msg
treeToolbar tree =
    styled S.div
        [ displayFlex
        , alignItems stretch
        , justifyContent stretch
        ]
        []
        [ Button.button
            [ Button.small
            , Button.light
            , Button.disabled (isTreeEmpty tree)
            , Button.onClick ClearTreeMsg
            , Button.attrs []
            ]
            [ S.text "Clear" |> S.toUnstyled
            ]
            |> S.fromUnstyled
        ]
