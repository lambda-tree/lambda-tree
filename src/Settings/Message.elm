module Settings.Message exposing (..)

import Settings.Model exposing (TypeSystem)


type Msg
    = CheckErrorsChangedMsg Bool
    | TypeSystemChangedMsg TypeSystem
    | UseCombinedRulesChangedMsg Bool
    | SidebarVisibilityChangedMsg Bool
