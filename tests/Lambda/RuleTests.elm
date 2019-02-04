module Lambda.RuleTests exposing (..)

import Expect exposing (Expectation)
import Lambda.Expression exposing (..)
import Test exposing (..)
import Lambda.Rule exposing (TyRule(..), checkRule)


dummyTest : Test
dummyTest =
    describe "dummyTest"
        [ test "test1" <|
            \_ ->
                True
                    |> Expect.equal True
        ]


compilationTest : Test
compilationTest =
    describe "compilationTest"
        [ test "test if it compiles" <|
            \_ ->
                checkRule
                    |> \_ -> Expect.pass
        ]


checkRuleTest : Test
checkRuleTest =
    describe "checkRule"
        [ test "test1" <|
            \_ ->
                checkRule
                    (TVar
                        { bottomCtx = [ ( "x", VarBind (TyName "Bool") ) ]
                        , bottomTerm = (TmVar I 0 1)
                        , bottomTy = (TyName "Bool")
                        , topCtx = [ ( "x", VarBind (TyName "Bool") ) ]
                        , topTerm = (TmVar I 0 1)
                        , topTy = (TyName "Bool")
                        }
                    )
                    |> Expect.equal True
        ]
