module RuleTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Lambda exposing (..)
import Rule exposing (..)
import Context exposing (..)


checkRuleTest : Test
checkRuleTest =
    describe "checkRule"
        [ test "test1" <|
            \_ ->
                checkRule
                    (TVar
                        [ ( "x", VarBind (TyName "Bool") ) ]
                        (TmVar I 0 1)
                        (TyName "Bool")
                        [ ( "x", VarBind (TyName "Bool") ) ]
                        (TmVar I 0 1)
                        (TyName "Bool")
                        Nothing
                    )
                    |> Expect.equal True
        ]
