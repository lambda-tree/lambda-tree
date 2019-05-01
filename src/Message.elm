module Message exposing (..)

import RuleTree.Message
import Settings.Message


type Msg
    = RuleTreeMsg RuleTree.Message.Msg
    | SettingsMsg Settings.Message.Msg
