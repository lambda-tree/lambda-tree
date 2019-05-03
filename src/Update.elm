module Update exposing (..)

import Bootstrap.Modal as Modal
import File
import File.Download
import File.Select
import Message exposing (..)
import Model exposing (..)
import RuleTree.Encode
import RuleTree.Message
import RuleTree.Update exposing (doSubstitution)
import Settings.Update
import Substitutor.Init
import Substitutor.Update
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubstitutionMsg m ->
            ( { model | substitution = Substitutor.Update.update m model.substitution }
            , Cmd.none
            )

        RuleTreeMsg m ->
            case m of
                RuleTree.Message.OpenSubstitutionMsg path ->
                    ( { model
                        | substitutionPath = Just path
                        , substitutionModal = Modal.shown
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | ruleTree = RuleTree.Update.update m model.settings model.ruleTree }
                    , Cmd.none
                    )

        SettingsMsg m ->
            ( { model | settings = Settings.Update.update m model.settings }
            , Cmd.none
            )

        HideSubstitutionModalMsg ->
            ( { model | substitutionModal = Modal.hidden }
            , Cmd.none
            )

        ShowSubstitutionModalMsg ->
            ( { model | substitutionModal = Modal.shown }
            , Cmd.none
            )

        DoSubstitutionMsg ->
            ( { model
                | ruleTree = doSubstitution model.substitution model.ruleTree
                , substitutionModal = Modal.hidden
                , substitution = Substitutor.Init.init
              }
            , Cmd.none
            )

        ImportFileMsg ->
            ( model, File.Select.file [ "application/json" ] FileImportedMsg )

        FileImportedMsg f ->
            ( model
            , File.toString f
                |> Task.perform RuleTreeImportedMsg
            )

        RuleTreeImportedMsg s ->
            Debug.log s
                ( model
                , Cmd.none
                )

        ExportMsg ->
            ( model
            , File.Download.string "tree.json" "application/json" (RuleTree.Encode.toString model.ruleTree)
            )
