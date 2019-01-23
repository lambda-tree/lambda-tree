module Context exposing (..)

import Lambda exposing (..)


type alias Context =
    List ContextObject


type alias ContextObject =
    ( String, Binding )
