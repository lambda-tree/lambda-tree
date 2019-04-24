module Main exposing (..)

import Browser
import Html.Styled exposing (toUnstyled)
import Init
import Message exposing (Msg)
import Model exposing (Model)
import Update
import View


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( Init.init, Cmd.none )
        , update = \msg -> Update.update msg >> (\m -> ( m, Cmd.none ))
        , view =
            \m ->
                { title = "Lambda Tree"
                , body = [ m |> View.view |> toUnstyled ]
                }

        --        , view = View.view >> toUnstyled
        , subscriptions = \_ -> Sub.none
        }
