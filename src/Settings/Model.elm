module Settings.Model exposing (..)

import Lambda.Expression exposing (TypeSystem)


type alias Model =
    { checkErrors : Bool
    , typeSystem : TypeSystem
    , showSidebar : Bool
    }
