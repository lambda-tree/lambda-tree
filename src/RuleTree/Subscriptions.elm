module RuleTree.Subscriptions exposing (..)

import Bootstrap.Dropdown as Dropdown
import RuleTree.Message exposing (Msg(..))
import RuleTree.Model exposing (RuleTree)
import Utils.Tree


subscriptions : RuleTree -> Sub Msg
subscriptions tree =
    tree
        |> Utils.Tree.map .dropdown
        |> Utils.Tree.indexedMap (\idx dropdown -> Dropdown.subscriptions dropdown (RuleDropdownMsg idx))
        |> Utils.Tree.toList
        |> Sub.batch
