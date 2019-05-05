module ErrorReport.Update exposing (..)

import ErrorReport.Message exposing (Msg(..))
import ErrorReport.Model exposing (Model)
import ErrorReport.Utils exposing (showError)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ErrorReportDismissMsg ->
            { model | text = Nothing }

        ErrorReportShowMsg text ->
            showError text
