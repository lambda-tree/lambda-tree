module RuleTree.Message exposing (..)

import RuleTree.Model exposing (Rule, TextKind)
import Substitutor.Model


type Msg
    = TextChangedMsg (List Int) TextKind String
    | HintMsg
    | RemoveMsg (List Int)
    | RuleSelectedMsg (List Int) Rule
    | RuleClickedMsg Rule
    | DoSubstitutionMsg Substitutor.Model.Model
