module RuleTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Lambda exposing (..)
--import Rule exposing (..)


exampleTest : Test
exampleTest =
    describe "exampleTest"
        [ test "exampleTest1" <|
            \_ ->
                True
                    |> Expect.equal True
        ]
