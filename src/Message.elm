module Message exposing (..)

import RuleTree.Message
import Settings.Message
import Substitutor.Message


type Msg
    = RuleTreeMsg RuleTree.Message.Msg
    | SubstitutionMsg Substitutor.Message.Msg
    | ShowSubstitutionModalMsg
    | DoSubstitutionMsg
    | HideSubstitutionModalMsg
    | SettingsMsg Settings.Message.Msg
