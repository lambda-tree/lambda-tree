module Model exposing (..)

import RuleTree.Model
import Settings.Model
import Substitutor.Model


type alias Model =
    { ruleTree : RuleTree.Model.RuleTree
    , settings : Settings.Model.Model
    }
