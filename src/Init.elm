module Init exposing (..)

import Material
import Model exposing (..)
import RuleTree.Init
import Substitutor.Init


init : Model
init =
    { ruleTree = RuleTree.Init.init
    , substitution = Substitutor.Init.init
    , showErrors = True
    , mdc = Material.defaultModel
    }
