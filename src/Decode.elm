module Decode exposing (..)

import Json.Decode as D
import Lambda.Expression exposing (HMFlavor(..), TypeSystem(..))
import RuleTree.Decode exposing (ruleTreeDecoder)
import RuleTree.Model exposing (RuleTree)


type alias DecodeModel =
    { ruleTree : RuleTree, typeSystem : TypeSystem }


typeSystemDecoder : D.Decoder TypeSystem
typeSystemDecoder =
    D.int
        |> D.andThen
            (\i ->
                case i of
                    0 ->
                        D.succeed SimplyTyped

                    1 ->
                        D.succeed <| HM NonDeterministic

                    2 ->
                        D.succeed <| HM SyntaxDirected

                    3 ->
                        D.succeed SystemF

                    _ ->
                        D.fail <| "Invalid type system enum: " ++ String.fromInt i
            )


modelDecoder : D.Decoder DecodeModel
modelDecoder =
    D.map2 (\ruleTree typeSystem -> { ruleTree = ruleTree, typeSystem = typeSystem })
        (D.field "ruleTree" ruleTreeDecoder)
        (D.field "typeSystem" typeSystemDecoder)


fromString : String -> Result String DecodeModel
fromString =
    D.decodeString modelDecoder
        >> Result.mapError D.errorToString
