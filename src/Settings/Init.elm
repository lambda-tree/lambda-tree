module Settings.Init exposing (..)

import Lambda.Expression exposing (TypeSystem(..))
import Settings.Model exposing (Model)


init : Model
init =
    { checkErrors = True, typeSystem = SimplyTyped, useCombinedRules = False }
