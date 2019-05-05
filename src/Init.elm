module Init exposing (..)

import ErrorReport.Init
import Model exposing (Model)
import RuleTree.Init
import Settings.Init
import Substitutor.Init


init : Model
init =
    { ruleTree = RuleTree.Init.init
    , substitution = Substitutor.Init.init
    , settings = Settings.Init.init
    , errorReport = ErrorReport.Init.init
    }
