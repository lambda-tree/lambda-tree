module RuleTree.Message exposing (..)

import Bootstrap.Dropdown as Dropdown
import Bootstrap.Popover as Popover
import Lambda.Rule exposing (Rule)
import RuleTree.Model exposing (TextKind)


type Msg
    = TextChangedMsg (List Int) TextKind String
    | HintMsg
    | RemoveMsg (List Int)
    | SelectRuleMsg (List Int)
    | RuleSelectedMsg (List Int) Rule
    | RuleClickedMsg Rule
    | RuleDropdownMsg (List Int) Dropdown.State
    | RuleStatusPopoverMsg (List Int) Popover.State
    | OpenSubstitutionMsg (List Int)
