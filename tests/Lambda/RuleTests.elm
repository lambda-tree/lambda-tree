module Lambda.RuleTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Lambda.Expression exposing (..)


--import Lambda.Rule exposing (..)

import Lambda.Context exposing (..)


dummyTest : Test
dummyTest =
    describe "dummyTest"
        [ test "test1" <|
            \_ ->
                True
                    |> Expect.equal True
        ]



--
--checkRuleTest : Test
--checkRuleTest =
--    describe "checkRule"
--        [ test "test1" <|
--            \_ ->
--                checkRule
--                    (TVar
--                        [ ( "x", VarBind (TyName "Bool") ) ]
--                        (TmVar I 0 1)
--                        (TyName "Bool")
--                        [ ( "x", VarBind (TyName "Bool") ) ]
--                        (TmVar I 0 1)
--                        (TyName "Bool")
--                        Nothing
--                    )
--                    |> Expect.equal True
--        ]
