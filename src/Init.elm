module Init exposing (..)

import Bootstrap.Modal as Modal
import Model exposing (..)
import RuleTree.Init
import Settings.Init
import Substitutor.Init


init : Model
init =
    { ruleTree = RuleTree.Init.init
    , substitution = Substitutor.Init.init
    , substitutionPath = Nothing
    , substitutionModal = Modal.hidden
    , settings = Settings.Init.init
    }
