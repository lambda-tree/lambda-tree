module RuleTree.Decode exposing (..)

import Json.Decode as D
import Lambda.Rule exposing (Rule(..))
import RuleTree.Model exposing (..)
import Utils.Tree exposing (Tree, treeDecoder)


ruleDecoder : D.Decoder Rule
ruleDecoder =
    D.string
        |> D.andThen
            (\r ->
                case r of
                    "TTrue" ->
                        D.succeed TTrue

                    "TFalse" ->
                        D.succeed TFalse

                    "TVar" ->
                        D.succeed TVar

                    "TVarInst" ->
                        D.succeed TVarInst

                    "TAbs" ->
                        D.succeed TAbs

                    "TApp" ->
                        D.succeed TApp

                    "TIf" ->
                        D.succeed TIf

                    "TTAbs" ->
                        D.succeed TTAbs

                    "TTApp" ->
                        D.succeed TTApp

                    "TLet" ->
                        D.succeed TLet

                    "TLetGen" ->
                        D.succeed TLetGen

                    "TGen" ->
                        D.succeed TGen

                    "TInst" ->
                        D.succeed TInst

                    "NoRule" ->
                        D.succeed NoRule

                    _ ->
                        D.fail <| "Invalid rule type: " ++ r
            )


ruleTreeContentDecoder : D.Decoder RuleTreeContent
ruleTreeContentDecoder =
    D.map4 (\ctx term ty rule -> { emptyTreeContent | ctx = ctx, term = term, ty = ty, rule = rule })
        (D.field "ctx" D.string)
        (D.field "term" D.string)
        (D.field "ty" D.string)
        (D.field "rule" ruleDecoder)


ruleTreeDecoder : D.Decoder RuleTree
ruleTreeDecoder =
    treeDecoder ruleTreeContentDecoder


fromString : String -> Result String RuleTree
fromString =
    D.decodeString ruleTreeDecoder
        >> Result.mapError D.errorToString
