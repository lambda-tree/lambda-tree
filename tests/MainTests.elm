module MainTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (main)


compilationTest : Test
compilationTest =
    describe "compilationTest"
        [ test "test if it compiles" <|
            \_ ->
                main
                    |> \_ -> Expect.pass
        ]
