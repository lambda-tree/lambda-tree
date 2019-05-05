module Subscriptions exposing (..)

import Bootstrap.Dropdown as Dropdown
import Message exposing (Msg(..))
import Model exposing (Model)
import RuleTree.Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ RuleTree.Subscriptions.subscriptions model.ruleTree
            |> Sub.map RuleTreeMsg
        , Dropdown.subscriptions model.exportDropdown ExportDropdownMsg
        ]
