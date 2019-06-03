module Settings.Update exposing (..)

import Settings.Message exposing (Msg(..))
import Settings.Model exposing (Model)
import Settings.Utils exposing (setTypeSystem)


update : Msg -> Model -> Model
update msg model =
    case msg of
        CheckErrorsChangedMsg value ->
            { model | checkErrors = value }

        TypeSystemChangedMsg value ->
            setTypeSystem value model

        SidebarVisibilityChangedMsg value ->
            { model | showSidebar = value }

        SidebarSectionChangedMsg section ->
            { model | sidebarSection = section }
