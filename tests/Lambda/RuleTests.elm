module Lambda.RuleTests exposing (..)

import Expect exposing (Expectation)
import Lambda.Expression exposing (..)
import Lambda.Rule exposing (TyRule(..), checkRule)
import Test exposing (..)


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
                    |> (\_ -> Expect.pass)
        ]


checkRuleTest : Test
checkRuleTest =
    describe "checkRule"
        [ test "test1" <|
            \_ ->
                checkRule
                    (TVar
                        { bottom =
                            { ctx =
                                [ ( "x"
                                  , VarBind
                                        (TyName "Bool")
                                  )
                                ]
                            , term = TmVar I 0 1
                            , ty = TyName "Bool"
                            }
                        , top =
                            { ctx =
                                [ ( "x"
                                  , VarBind
                                        (TyName "Bool")
                                  )
                                ]
                            , term = TmVar I 0 1
                            , ty = TyName "Bool"
                            }
                        }
                    )
                    |> Expect.equal True
        ]
