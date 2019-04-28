module Main exposing (..)

import Bootstrap.CDN
import Browser
import Html.Styled exposing (toUnstyled)
import Init
import Message exposing (Msg(..))
import Model exposing (Model)
import Update
import View
import View.GlobalCss


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( Init.init, Cmd.none )
        , update = \msg model -> ( Update.update msg model, Cmd.none )
        , view =
            \model ->
                { title = "Lambda Tree"
                , body =
                    [ View.GlobalCss.style |> toUnstyled
                    , View.view model |> toUnstyled
                    ]
                }
        , subscriptions = \_ -> Sub.none
        }
