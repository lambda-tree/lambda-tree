port module Ports exposing (..)

import Json.Encode as E


port cache : E.Value -> Cmd msg


port log : String -> Cmd msg
