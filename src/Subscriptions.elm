module Subscriptions exposing (..)

import Message exposing (Msg(..))
import Model exposing (Model)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
