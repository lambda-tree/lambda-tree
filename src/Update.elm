module Update exposing (..)

import Message exposing (..)
import Model exposing (..)
import RuleTree.Update
import Settings.Update
import Substitutor.Update


update : Msg -> Model -> Model
update msg model =
    case Debug.log "update :: Msg" msg of
        RuleTreeMsg m ->
            { model | ruleTree = RuleTree.Update.update m model.ruleTree }

        SettingsMsg m ->
            { model | settings = Settings.Update.update m model.settings }



--        _ ->
--            model
