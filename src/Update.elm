module Update exposing (..)

import Bootstrap.Modal as Modal
import Message exposing (..)
import Model exposing (..)
import RuleTree.Message
import RuleTree.Update exposing (doSubstitution)
import Settings.Update
import Substitutor.Init
import Substitutor.Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        SubstitutionMsg m ->
            { model | substitution = Substitutor.Update.update m model.substitution }

        RuleTreeMsg m ->
            case m of
                RuleTree.Message.OpenSubstitutionMsg path ->
                    { model
                        | substitutionPath = Just path
                        , substitutionModal = Modal.shown
                    }

                _ ->
                    { model | ruleTree = RuleTree.Update.update m model.settings model.ruleTree }

        SettingsMsg m ->
            { model | settings = Settings.Update.update m model.settings }

        HideSubstitutionModalMsg ->
            { model | substitutionModal = Modal.hidden }

        ShowSubstitutionModalMsg ->
            { model | substitutionModal = Modal.shown }

        DoSubstitutionMsg ->
            { model
                | ruleTree = doSubstitution model.substitution model.ruleTree
                , substitutionModal = Modal.hidden
                , substitution = Substitutor.Init.init
            }
