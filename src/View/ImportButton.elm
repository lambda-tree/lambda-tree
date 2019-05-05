module View.ImportButton exposing (..)

import Bootstrap.Button as Button
import Html
import Html.Styled as S
import Message exposing (Msg(..))


importButton =
    Button.button
        [ Button.small
        , Button.dark
        , Button.onClick ImportJsonMsg
        ]
        [ Html.text "Import" ]
        |> S.fromUnstyled
