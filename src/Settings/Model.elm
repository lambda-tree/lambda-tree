module Settings.Model exposing (..)

import Lambda.Expression


type alias Model =
    { checkErrors : Bool
    , typeSystem : Lambda.Expression.TypeSystem
    , useCombinedRules : Bool
    }
