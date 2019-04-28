module Message exposing (..)

import RuleTree.Message
import Substitutor.Message


type TypeSystem
    = SimplyTyped
    | HM
    | SystemF


type Msg
    = RuleTreeMsg RuleTree.Message.Msg
    | SubstitutionMsg Substitutor.Message.Msg
    | ToggleChecking Bool
    | SelectTypeSystemMsg TypeSystem
