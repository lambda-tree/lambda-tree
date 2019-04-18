module Init exposing (..)

import Model exposing (..)
import Substitutor.Init


init : Model
init =
    { tree = emptyTree
    , zoomLevel = 100
    , substitution = Substitutor.Init.init
    }
