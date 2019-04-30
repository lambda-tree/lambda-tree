module Settings.Utils exposing (..)

import Lambda.Expression
import Settings.Model exposing (Model, TypeSystem(..))


getTypeSystem : Model -> Lambda.Expression.TypeSystem
getTypeSystem model =
    case model.typeSystem of
        SimplyTyped ->
            Lambda.Expression.SimplyTyped

        HM ->
            if model.useCombinedRules then
                Lambda.Expression.HM Lambda.Expression.SyntaxDirected

            else
                Lambda.Expression.HM Lambda.Expression.NonDeterministic

        SystemF ->
            Lambda.Expression.SystemF
