module Settings.Message exposing (..)

import Lambda.Expression exposing (TypeSystem)
import Settings.Model exposing (SidebarSection)


type Msg
    = CheckErrorsChangedMsg Bool
    | TypeSystemChangedMsg TypeSystem
    | SidebarVisibilityChangedMsg Bool
    | SidebarSectionChangedMsg SidebarSection
