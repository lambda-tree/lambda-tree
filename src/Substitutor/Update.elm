module Substitutor.Update exposing (..)

import Substitutor.Message exposing (Msg(..))
import Substitutor.Model exposing (Model)


update : Msg -> Model -> Model
update msg model =
    case msg of
        TyChangedMsg s ->
            { model | ty = s }

        VarChangedMsg s ->
            { model | var = s }
