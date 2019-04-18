module Substitutor.Update exposing (..)

import Substitutor.Message exposing (Msg(..))
import Substitutor.Model exposing (Model)


update : Msg -> Model -> Model
update msg model =
    case msg of
        TyChanged s ->
            { model | ty = s }

        VarChanged s ->
            { model | var = s }
