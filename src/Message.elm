module Message exposing (..)

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
    | FileImportedMsg File
    | ImportFileMsg
    | RuleTreeImportedMsg String
    | ExportMsg
    | ErrorReportMsg ErrorReport.Message.Msg
