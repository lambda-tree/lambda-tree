module Init exposing (..)

import Model exposing (..)
import RuleTree.Init
import Settings.Init
import Substitutor.Init


init : Model
init =
    { ruleTree = RuleTree.Init.init
    , settings = Settings.Init.init
    }
