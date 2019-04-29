module RuleTree.Model exposing (..)

import Bootstrap.Dropdown as Dropdown
import Lambda.Rule exposing (Rule(..))
import Utils.Tree exposing (Tree(..))


type TextKind
    = CtxKind
    | TermKind
    | TyKind


type alias RuleTreeContent =
    { ctx : String, term : String, ty : String, rule : Rule, dropdown : Dropdown.State }


type alias RuleTree =
    Tree RuleTreeContent


emptyTree : RuleTree
emptyTree =
    Node { ctx = "", term = "", ty = "", rule = NoRule, dropdown = Dropdown.initialState } []
