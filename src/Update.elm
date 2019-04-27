module Update exposing (..)

import Material
import Message exposing (..)
import Model exposing (..)
import RuleTree.Update
import Substitutor.Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update :: Msg" msg of
        SubstitutionMsg m ->
            ( { model | substitution = Substitutor.Update.update m model.substitution }, Cmd.none )

        RuleTreeMsg m ->
            ( { model | ruleTree = RuleTree.Update.update m model.ruleTree }, Cmd.none )

        Mdc msg_ ->
            Material.update Mdc msg_ model

        ToggleChecking ->
            ( { model | showErrors = not model.showErrors }, Cmd.none )

        _ ->
            ( model, Cmd.none )
