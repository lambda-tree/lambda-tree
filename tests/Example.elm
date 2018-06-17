module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Lambda exposing (..)


suite : Test
suite =
    describe "Simple test suite"
        [ test "Simple test case" <|
            \_ -> 1 + 1 |> Expect.equal 2

        -- , test "LambdaParser" <|
        --     \_ -> lambdaStrings (LVar "Hello") |> Expect.equal [ "Hello" ]
        , test "Shifting Type" <|
            \_ ->
                typeShiftAbove (-1) 0 (TyAll "alpha" (TyArr (TyVar 1 1) (TyVar 1 1)))
                    |> Expect.equal (TyAll "alpha" (TyArr (TyVar 0 0) (TyVar 0 0)))
        ]
