module Settings.Model exposing (..)


type TypeSystem
    = SimplyTyped
    | HM
    | SystemF


type alias Model =
    { checkErrors : Bool
    , typeSystem : TypeSystem
    , useCombinedRules : Bool
    }
