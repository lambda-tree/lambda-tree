module RuleTree.Model exposing (..)

import Bootstrap.Dropdown as Dropdown
import Bootstrap.Popover as Popover
import Lambda.Rule exposing (Rule(..))
import Utils.Tree exposing (Tree(..))


type TextKind
    = CtxKind
    | TermKind
    | TyKind


type alias RuleTreeContent =
    { ctx : String
    , term : String
    , ty : String
    , rule : Rule
    , dropdown : Dropdown.State
    , statusPopover : Popover.State
    }


type alias RuleTree =
    Tree RuleTreeContent


emptyTreeContent =
    { ctx = ""
    , term = ""
    , ty = ""
    , rule = NoRule
    , dropdown = Dropdown.initialState
    , statusPopover = Popover.initialState
    }


emptyTree : RuleTree
emptyTree =
    Node emptyTreeContent []
