module Settings.Message exposing (..)

import Lambda.Expression exposing (TypeSystem)


type Msg
    = CheckErrorsChangedMsg Bool
    | TypeSystemChangedMsg TypeSystem
    | UseCombinedRulesChangedMsg Bool
