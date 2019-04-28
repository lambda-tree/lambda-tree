module Model exposing (..)

import RuleTree.Model
import Substitutor.Model


type alias Model =
    { ruleTree : RuleTree.Model.RuleTree
    , substitution : Substitutor.Model.Model
    , showErrors : Bool
    }
