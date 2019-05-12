module Message exposing (..)

import Bootstrap.Dropdown as Dropdown
import ErrorReport.Message
import File exposing (File)
import RuleTree.Message
import Settings.Message
import Substitutor.Message


type Msg
    = RuleTreeMsg RuleTree.Message.Msg
    | SubstitutionMsg Substitutor.Message.Msg
    | SettingsMsg Settings.Message.Msg
    | ErrorReportMsg ErrorReport.Message.Msg
    | DoSubstitutionMsg
    | ImportJsonMsg
    | JsonImportedMsg File
    | ModelImportedMsg String
    | ExportDropdownMsg Dropdown.State
    | ExportLaTexMsg
    | ExportJsonMsg
