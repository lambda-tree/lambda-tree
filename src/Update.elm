module Update exposing (..)

import Decode
import Encode
import ErrorReport.Update
import ErrorReport.Utils exposing (showError)
import File
import File.Download
import File.Select
import Lambda.Show.LaTex
import Message exposing (..)
import Model exposing (..)
import RuleTree.Update exposing (doSubstitution)
import RuleTree.ViewModel exposing (getExprTree)
import Settings.Update
import Settings.Utils exposing (getTypeSystem, setTypeSystem)
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

        ErrorReportMsg m ->
            ( { model | errorReport = ErrorReport.Update.update m model.errorReport }
            , Cmd.none
            )

        RuleTreeMsg m ->
            ( { model | ruleTree = RuleTree.Update.update m model.settings model.ruleTree }
            , Cmd.none
            )

        SettingsMsg m ->
            ( { model | settings = Settings.Update.update m model.settings }
            , Cmd.none
            )

        DoSubstitutionMsg ->
            ( { model
                | ruleTree = doSubstitution model.substitution model.ruleTree
                , substitution = Substitutor.Init.init
              }
            , Cmd.none
            )

        ImportJsonMsg ->
            ( model, File.Select.file [ "application/json" ] JsonImportedMsg )

        JsonImportedMsg file ->
            ( model
            , File.toString file
                |> Task.perform ModelImportedMsg
            )

        ModelImportedMsg str ->
            case Decode.fromString str of
                Ok d ->
                    ( { model
                        | ruleTree = d.ruleTree
                        , settings = setTypeSystem d.typeSystem model.settings
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( { model | errorReport = showError e }, Cmd.none )

        ExportDropdownMsg dropdownState ->
            ( { model | exportDropdown = dropdownState }, Cmd.none )

        ExportJsonMsg ->
            ( model
            , File.Download.string "tree.json" "application/json" (Encode.toString model)
            )

        ExportLaTexMsg ->
            ( model
            , File.Download.string "tree.tex"
                "application/x-tex"
                (getExprTree (getTypeSystem model.settings) model.ruleTree
                    |> Lambda.Show.LaTex.showExprTree
                    |> Lambda.Show.LaTex.wrapProofTreeForExport
                )
            )
