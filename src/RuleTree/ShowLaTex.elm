module RuleTree.ShowLaTex exposing (..)

import Lambda.Expression exposing (TypeSystem)
import Lambda.Show.LaTex as LaTex
import Lambda.Show.Print exposing (Print(..), showCtx, showTerm, showType)
import RuleTree.Model exposing (RuleTree)
import RuleTree.ViewModel exposing (getExprTree)
import Utils.Tree


show : TypeSystem -> RuleTree -> String
show typeSystem tree =
    getExprTree typeSystem tree
        |> Utils.Tree.foldTree
            (\content subPrints ->
                case ( content.ctx, content.term, content.ty ) of
                    ( Ok ctx, Ok term, Ok ty ) ->
                        Imply (showCtx ctx) (OfType (showTerm ctx term) (showType ctx ty))
                            |> LaTex.show

                    _ ->
                        ""
            )
