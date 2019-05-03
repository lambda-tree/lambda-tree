module RuleTree.View.ExportButton exposing (..)

import Bootstrap.Button as Button
import Html
import Html.Attributes
import Html.Styled as S
import Message exposing (Msg(..))
import RuleTree.Encode


exportButton =
    Button.button
        [ Button.small
        , Button.dark
        , Button.onClick ExportMsg
        ]
        [ Html.text "Export" ]
        |> S.fromUnstyled
