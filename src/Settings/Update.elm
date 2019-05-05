module Settings.Update exposing (..)

import Settings.Message exposing (Msg(..))
import Settings.Model exposing (Model)


update : Msg -> Model -> Model
update msg model =
    case msg of
        CheckErrorsChangedMsg value ->
            { model | checkErrors = value }

        TypeSystemChangedMsg value ->
            { model | typeSystem = value }

        SidebarVisibilityChangedMsg value ->
            { model | showSidebar = value }
