module Main exposing (..)

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
        { init = \flags -> Init.init flags
        , update = \msg model -> Update.update msg model
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
