module Message exposing (..)

import Material
import RuleTree.Message
import Substitutor.Message


type Msg
    = RuleTreeMsg RuleTree.Message.Msg
    | SubstitutionMsg Substitutor.Message.Msg
    | Mdc (Material.Msg Msg)
