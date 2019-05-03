module Utils.TreeTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode as D
import Test exposing (..)
import Utils.Tree exposing (Tree(..), decodeTree)


decodeTreeTest : Test
decodeTreeTest =
    describe "decodeTree"
        [ test "should decode single node int tree" <|
            \_ ->
                D.decodeString (decodeTree D.int) """
                    {
                        "content": 4,
                        "children":[]
                    }
                """
                    |> Expect.equal (Ok <| Node 4 [])
        , test "should decode deep int tree" <|
            \_ ->
                D.decodeString (decodeTree D.int) """
                    {
                        "content": 1,
                        "children":[
                            {
                                "content": 2,
                                "children":[
                                    {
                                        "content": 4,
                                        "children":[
                                            {
                                                "content": 5,
                                                "children":[
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            },
                            {
                                "content": 3,
                                "children":[
                                    {
                                        "content": 6,
                                        "children":[
                                        ]
                                    }
                                ]
                            }
                        ]
                    }
                """
                    |> Expect.equal
                        (Ok <|
                            Node 1
                                [ Node 2 [ Node 4 [ Node 5 [] ] ]
                                , Node 3 [ Node 6 [] ]
                                ]
                        )
        ]
