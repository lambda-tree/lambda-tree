module Message exposing (..)

import Material
import RuleTree.Message
import Substitutor.Message


type TypeSystem
    = SimplyTyped
    | HM
    | SystemF


type Msg
    = RuleTreeMsg RuleTree.Message.Msg
    | SubstitutionMsg Substitutor.Message.Msg
    | ToggleChecking
    | SelectTypeSystemMsg TypeSystem
    | Mdc (Material.Msg Msg)
