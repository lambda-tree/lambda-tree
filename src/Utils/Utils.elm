module Utils.Utils exposing (..)


extractError : Result e a -> Maybe e
extractError result =
    case result of
        Err e ->
            Just e

        Ok _ ->
            Nothing
