module Model exposing (..)

import ErrorReport.Model
import RuleTree.Model
import Settings.Model
import Substitutor.Model


type alias Model =
    { ruleTree : RuleTree.Model.RuleTree
    , substitution : Substitutor.Model.Model
    , settings : Settings.Model.Model
    , errorReport : ErrorReport.Model.Model
    }
