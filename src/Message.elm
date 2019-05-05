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
    | DoSubstitutionMsg
    | SettingsMsg Settings.Message.Msg
    | ErrorReportMsg ErrorReport.Message.Msg
    | ImportJsonMsg
    | JsonImportedMsg File
    | RuleTreeImportedMsg String
    | ExportDropdownMsg Dropdown.State
    | ExportLaTexMsg
    | ExportJsonMsg
