module Main exposing (main)

import Browser
import Html.Styled exposing (toUnstyled)
import Init
import Json.Decode as D
import Message exposing (Msg(..))
import Model exposing (Model)
import Subscriptions
import Update
import View
import View.GlobalCss


main : Program D.Value Model Msg
main =
    Browser.document
        { init = Init.init
        , update = Update.update
        , view =
            \model ->
                { title = "Lambda Tree"
                , body =
                    [ View.GlobalCss.style |> toUnstyled
                    , View.view model |> toUnstyled
                    ]
                }
        , subscriptions = Subscriptions.subscriptions
        }
