module Init exposing (..)

import Model exposing (..)


init : Model
init =
    { tree = emptyTree, zoomLevel = 100 }
