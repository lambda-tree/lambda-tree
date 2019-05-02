module Utils.Utils exposing (..)

import Html exposing (..)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json


extractError : Result e a -> Maybe e
extractError result =
    case result of
        Err e ->
            Just e

        Ok _ ->
            Nothing


onKeydownEnter : msg -> Attribute msg
onKeydownEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)
