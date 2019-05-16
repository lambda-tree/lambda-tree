module RuleTree.ShowLaTex exposing (..)

import Lambda.Expression exposing (TypeSystem)
import Lambda.Show.LaTex
import RuleTree.Model exposing (RuleTree)
import RuleTree.ViewModel exposing (getExprTree)


show : TypeSystem -> RuleTree -> String
show typeSystem tree =
    getExprTree typeSystem tree
        |> Lambda.Show.LaTex.showExprTree
        |> Lambda.Show.LaTex.wrapProofTreeForExport
