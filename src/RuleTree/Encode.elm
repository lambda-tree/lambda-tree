module RuleTree.Encode exposing (..)

import Base64
import Json.Encode as E exposing (..)
import Lambda.Rule exposing (Rule(..))
import RuleTree.Model exposing (..)
import Utils.Tree exposing (Tree, encodeTree)


encodeRule : Rule -> E.Value
encodeRule r =
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


encodeRuleTreeContent : RuleTreeContent -> E.Value
encodeRuleTreeContent c =
    E.object
        [ ( "ctx", E.string c.ctx )
        , ( "term", E.string c.term )
        , ( "ty", E.string c.ty )
        , ( "rule", encodeRule c.rule )
        ]


encodeRuleTree : RuleTree -> E.Value
encodeRuleTree =
    encodeTree encodeRuleTreeContent


toString : RuleTree -> String
toString =
    encodeRuleTree
        >> E.encode 2


toBase64String : RuleTree -> String
toBase64String =
    encodeRuleTree
        >> E.encode 0
        >> Base64.encode
