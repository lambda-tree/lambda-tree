module Settings.Model exposing (..)

import Lambda.Expression exposing (TypeSystem)


type SidebarSection
    = RulesSection
    | HelpSection


type alias Model =
    { checkErrors : Bool
    , typeSystem : TypeSystem
    , showSidebar : Bool
    , sidebarSection : SidebarSection
    }
