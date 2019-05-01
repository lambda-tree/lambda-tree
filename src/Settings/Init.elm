module Settings.Init exposing (..)

import Settings.Model exposing (Model, TypeSystem(..))


init : Model
init =
    { checkErrors = True
    , typeSystem = SimplyTyped
    , useCombinedRules = False
    , showSidebar = True
    }
