module Lambda.ErrorHandling exposing (..)

import Expect exposing (Expectation)
import Lambda.Parse exposing (..)
import Result
import Test exposing (..)


parseTermTest : Test
parseTermTest =
    describe "parseTerm"
        [ test "should return error if there's a parsing problem" <|
            \_ ->
                parseTerm "termVar1 F"
                    |> Expect.equal (Result.Err <| { row = 1, col = 10 })
        ]
