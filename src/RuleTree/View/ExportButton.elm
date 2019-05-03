module RuleTree.View.ExportButton exposing (..)

import Bootstrap.Button as Button
import Html
import Html.Attributes
import Html.Styled as S
import RuleTree.Encode


exportButton ruleTree =
    Button.linkButton
        [ Button.small
        , Button.dark
        , Button.attrs
            [ Html.Attributes.download "tree.json"
            , Html.Attributes.href
                ("data:application/octet-stream;charset=utf-8;base64,"
                    ++ RuleTree.Encode.toBase64String ruleTree
                )
            ]
        ]
        [ Html.text "Export" ]
        |> S.fromUnstyled
