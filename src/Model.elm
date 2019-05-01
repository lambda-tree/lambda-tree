module Model exposing (..)

import Bootstrap.Modal as Modal
import RuleTree.Model
import Settings.Model
import Substitutor.Model


type alias Model =
    { ruleTree : RuleTree.Model.RuleTree
    , substitution : Substitutor.Model.Model
    , substitutionPath : Maybe (List Int)
    , substitutionModal : Modal.Visibility
    , settings : Settings.Model.Model
    }
