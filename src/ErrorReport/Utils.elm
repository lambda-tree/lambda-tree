module ErrorReport.Utils exposing (..)

import Bootstrap.Modal as Modal
import ErrorReport.Model exposing (Model)


getModalState : Model -> Modal.Visibility
getModalState { text } =
    case text of
        Just _ ->
            Modal.shown

        Nothing ->
            Modal.hidden


showError : String -> Model -> Model
showError text model =
    { model | text = Just text }
