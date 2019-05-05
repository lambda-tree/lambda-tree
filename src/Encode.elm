module Encode exposing (..)

import Json.Encode as E exposing (..)
import Lambda.Expression exposing (HMFlavor(..), TypeSystem(..))
import Model
import RuleTree.Encode exposing (ruleTreeEncoder)
import RuleTree.Model exposing (RuleTree)
import Settings.Model


type alias Encode =
    { ruleTree : RuleTree, settings : Settings.Model.Model }


typeSystemEncoder : TypeSystem -> E.Value
typeSystemEncoder ts =
    case ts of
        SimplyTyped ->
            E.int 0

        HM NonDeterministic ->
            E.int 1

        HM SyntaxDirected ->
            E.int 2

        SystemF ->
            E.int 3


modelEncoder : Model.Model -> E.Value
modelEncoder model =
    E.object
        [ ( "ruleTree", ruleTreeEncoder model.ruleTree )
        , ( "typeSystem", typeSystemEncoder model.settings.typeSystem )
        ]


toString : Model.Model -> String
toString =
    modelEncoder
        >> E.encode 2
