module RuleTree.Model exposing (..)

import Utils.Tree exposing (Tree(..))


type Rule
    = TTrue
    | TFalse
    | TVar
    | TVarInst
    | TAbs
    | TApp
    | TIf
    | TTAbs
    | TTApp
    | TLet
    | TLetGen
    | TGen
    | TInst
    | NoRule


type TextKind
    = CtxKind
    | TermKind
    | TyKind


type alias RuleTreeContent =
    { ctx : String, term : String, ty : String, rule : Rule }


type alias RuleTree =
    Tree RuleTreeContent


emptyTree : RuleTree
emptyTree =
    Node { ctx = "", term = "", ty = "", rule = NoRule } []
