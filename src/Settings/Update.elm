module Settings.Update exposing (..)

import Settings.Message exposing (Msg(..))
import Settings.Model exposing (Model)


update : Msg -> Model -> Model
update msg model =
    case msg of
        CheckErrorsChangedMsg value ->
            { model | checkErrors = value }

        UseCombinedRulesChangedMsg value ->
            { model | useCombinedRules = value }

        TypeSystemChangedMsg value ->
            { model | typeSystem = value }
