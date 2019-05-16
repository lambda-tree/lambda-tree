module Update exposing (..)

import Decode exposing (DecodeModel)
import Encode exposing (modelEncoder)
import ErrorReport.Update
import ErrorReport.Utils exposing (showError)
import File
import File.Download
import File.Select
import Message exposing (..)
import Model exposing (..)
import Ports exposing (cache)
import RuleTree.ShowLaTex
import RuleTree.Update exposing (doSubstitution)
import Settings.Update
import Settings.Utils exposing (getTypeSystem, setTypeSystem)
import Substitutor.Init
import Substitutor.Update
import Task
import Utils.Outcome as Outcome


cacheModel : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
cacheModel ( model, command ) =
    ( model, Cmd.batch [ command, cache (modelEncoder model) ] )


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
            let
                updatedRuleTree =
                    RuleTree.Update.update m model.settings model.ruleTree

                errorReport =
                    updatedRuleTree
                        |> Outcome.error
                        |> Maybe.map showError
                        |> Maybe.withDefault model.errorReport
            in
            ( { model
                | ruleTree = updatedRuleTree |> Outcome.value
                , errorReport = errorReport
              }
            , Cmd.none
            )
                |> cacheModel

        SettingsMsg m ->
            ( { model | settings = Settings.Update.update m model.settings }
            , Cmd.none
            )
                |> cacheModel

        DoSubstitutionMsg ->
            ( { model
                | ruleTree = doSubstitution model.substitution model.ruleTree
                , substitution = Substitutor.Init.init
              }
            , Cmd.none
            )
                |> cacheModel

        ImportJsonMsg ->
            ( model, File.Select.file [ "application/json" ] JsonImportedMsg )

        JsonImportedMsg file ->
            ( model
            , File.toString file
                |> Task.perform ModelImportedMsg
            )

        ModelImportedMsg str ->
            case Decode.fromString str of
                Ok decodeModel ->
                    ( updateWithDecodeModel decodeModel model
                    , Cmd.none
                    )
                        |> cacheModel

                Err e ->
                    ( { model | errorReport = showError e }, Cmd.none )

        ExportDropdownMsg dropdownState ->
            ( { model | exportDropdown = dropdownState }, Cmd.none )

        ExportJsonMsg ->
            ( model
            , File.Download.string "tree.json" "application/json" <|
                Encode.toString model
            )

        ExportLaTexMsg ->
            ( model
            , File.Download.string "tree.tex" "application/x-tex" <|
                RuleTree.ShowLaTex.show
                    (getTypeSystem model.settings)
                    model.ruleTree
            )


updateWithDecodeModel : DecodeModel -> Model -> Model
updateWithDecodeModel decodeModel model =
    { model
        | ruleTree = decodeModel.ruleTree
        , settings = setTypeSystem decodeModel.typeSystem model.settings
    }
