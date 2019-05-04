module RuleTree.Encode exposing (..)

import Json.Encode as E exposing (..)
import Lambda.Rule exposing (Rule(..))
import RuleTree.Model exposing (..)
import Utils.Tree exposing (Tree, treeEncoder)


ruleEncoder : Rule -> E.Value
ruleEncoder r =
    case r of
        TTrue ->
            E.string "TTrue"

        TFalse ->
            E.string "TFalse"

        TVar ->
            E.string "TVar"

        TVarInst ->
            E.string "TVarInst"

        TAbs ->
            E.string "TAbs"

        TApp ->
            E.string "TApp"

        TIf ->
            E.string "TIf"

        TTAbs ->
            E.string "TTAbs"

        TTApp ->
            E.string "TTApp"

        TLet ->
            E.string "TLet"

        TLetGen ->
            E.string "TLetGen"

        TGen ->
            E.string "TGen"

        TInst ->
            E.string "TInst"

        NoRule ->
            E.string "NoRule"


ruleTreeContentEncoder : RuleTreeContent -> E.Value
ruleTreeContentEncoder c =
    E.object
        [ ( "ctx", E.string c.ctx )
        , ( "term", E.string c.term )
        , ( "ty", E.string c.ty )
        , ( "rule", ruleEncoder c.rule )
        ]


ruleTreeEncoder : RuleTree -> E.Value
ruleTreeEncoder =
    treeEncoder ruleTreeContentEncoder


toString : RuleTree -> String
toString =
    ruleTreeEncoder
        >> E.encode 2
