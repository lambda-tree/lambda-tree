module ViewModel exposing (..)

import Lambda.Rule exposing (tryRule)
import Model exposing (Rule, Tree(..), TreeModel)


type alias TreeViewData =
    Tree { ctx : String, term : String, ty : String, rule : Rule, result : String }


getTreeViewData : TreeModel -> TreeViewData
getTreeViewData t =
    case t of
        Node { ctx, term, ty, rule } children ->
            Node { ctx = ctx, term = term, ty = ty, rule = rule, result = tryRule t } <| List.map getTreeViewData children
