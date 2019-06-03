module Settings.Init exposing (..)

import Lambda.Expression exposing (TypeSystem(..))
import Settings.Model exposing (Model, SidebarSection(..))


init : Model
init =
    { checkErrors = True
    , typeSystem = SimplyTyped
    , showSidebar = True
    , sidebarSection = RulesSection
    }
