module Lambda.Context exposing (..)

import Lambda.Expression exposing (..)


type alias Context =
    List ContextObject


type alias ContextObject =
    ( String, Binding )
