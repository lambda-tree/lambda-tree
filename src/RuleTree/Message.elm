module RuleTree.Message exposing (..)

import Bootstrap.Dropdown as Dropdown
import Lambda.Rule exposing (Rule)
import RuleTree.Model exposing (TextKind)
import Substitutor.Model


type Msg
    = TextChangedMsg (List Int) TextKind String
    | HintMsg
    | RemoveMsg (List Int)
    | SelectRuleMsg (List Int)
    | RuleSelectedMsg (List Int) Rule
    | RuleClickedMsg Rule
    | DoSubstitutionMsg Substitutor.Model.Model
    | RuleDropdownMsg (List Int) Dropdown.State
