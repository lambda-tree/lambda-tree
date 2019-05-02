module Lambda.InfererTests exposing (..)

import Expect exposing (Expectation)
import Inferer.Inferer exposing (inferTree)
import Lambda.Expression exposing (..)
import Lambda.Rule exposing (Rule(..))
import Test exposing (..)
import Utils.Tree exposing (Tree(..))


buildTreeTest : Test
buildTreeTest =
    describe "buildTree"
        [ test "should build tree" <|
            \_ ->
                inferTree (HM SyntaxDirected) [] (TmConst I TmTrue)
                    |> Expect.equal (Ok <| Node { ctx = [], term = TmConst I TmTrue, ty = TyConst TyBool, rule = TTrue, ss = [] } [])
        ]
