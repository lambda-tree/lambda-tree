module Subscriptions exposing (..)

import Material
import Message exposing (Msg(..))
import Model exposing (Model)


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
