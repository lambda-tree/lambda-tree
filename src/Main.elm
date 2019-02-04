module Main exposing (..)

import Browser
import Html.Styled exposing (toUnstyled)
import Init
import Update
import View
import Model
import Message


main : Program () Model.Model Message.Msg
main =
    Browser.sandbox
        { init = Init.init
        , update = Update.update
        , view = View.view >> toUnstyled
        }
