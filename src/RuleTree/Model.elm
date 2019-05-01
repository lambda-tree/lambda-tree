module RuleTree.Model exposing (..)

import Bootstrap.Dropdown as Dropdown
import Bootstrap.Popover as Popover
import Lambda.Rule exposing (Rule(..))
import Substitutor.Init
import Substitutor.Model
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
    , substitution : Substitutor.Model.Model
    }


type alias RuleTree =
    Tree RuleTreeContent


emptyTree : RuleTree
emptyTree =
    Node
        { ctx = ""
        , term = ""
        , ty = ""
        , rule = NoRule
        , dropdown = Dropdown.initialState
        , statusPopover = Popover.initialState
        , substitution = Substitutor.Init.init
        }
        []
