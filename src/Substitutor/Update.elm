module Substitutor.Update exposing (..)

import Lambda.Parse exposing (preprocess)
import Substitutor.Message exposing (Msg(..))
import Substitutor.Model exposing (Model)


update : Msg -> Model -> Model
update msg model =
    case msg of
        TyChanged s ->
            { model | ty = preprocess s }

        VarChanged s ->
            { model | var = s }
