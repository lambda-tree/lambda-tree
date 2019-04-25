module Main exposing (..)

import Browser
import Html.Styled exposing (toUnstyled)
import Init
import Material
import Message exposing (Msg(..))
import Model exposing (Model)
import Subscriptions exposing (subscriptions)
import Update
import View


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( Init.init, Material.init Mdc )
        , update = Update.update
        , view =
            \m ->
                { title = "Lambda Tree"
                , body = [ m |> View.view |> toUnstyled ]
                }
        , subscriptions = subscriptions
        }
