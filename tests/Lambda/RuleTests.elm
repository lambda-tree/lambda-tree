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
        [ test "TVar" <|
            \_ ->
                checkRule
                    (TVar
                        { bottom =
                            { ctx = [ ( "x", VarBind <| TyName "Bool" ) ]
                            , term = TmVar I 0 1
                            , ty = TyName "Bool"
                            }
                        , top =
                            { ctx = [ ( "x", VarBind <| TyName "Bool" ) ]
                            , term = TmVar I 0 1
                            , ty = TyName "Bool"
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TIf" <|
            \_ ->
                checkRule
                    (TIf
                        { bottom =
                            { ctx = [ ( "X", TyVarBind ), ( "y", VarBind <| TyName "Bool" ), ( "x", VarBind <| TyVar 1 2 ) ]
                            , term = TmIf I (TmVar I 1 3) (TmVar I 0 3) (TmApp I (TmAbs I "x" (TyName "Bool") (TmVar I 1 4)) (TmConst I TmTrue))
                            , ty = TyVar 2 3
                            }
                        , top1 =
                            { ctx = [ ( "X", TyVarBind ), ( "y", VarBind <| TyName "Bool" ), ( "x", VarBind <| TyVar 1 2 ) ]
                            , term = TmVar I 1 3
                            , ty = TyName "Bool"
                            }
                        , top2 =
                            { ctx = [ ( "X", TyVarBind ), ( "y", VarBind <| TyName "Bool" ), ( "x", VarBind <| TyVar 1 2 ) ]
                            , term = TmVar I 0 3
                            , ty = TyVar 2 3
                            }
                        , top3 =
                            { ctx = [ ( "X", TyVarBind ), ( "y", VarBind <| TyName "Bool" ), ( "x", VarBind <| TyVar 1 2 ) ]
                            , term = TmApp I (TmAbs I "x" (TyName "Bool") (TmVar I 1 4)) (TmConst I TmTrue)
                            , ty = TyVar 2 3
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TTrue" <|
            \_ ->
                checkRule
                    (TTrue
                        { bottom =
                            { ctx = [ ( "X", TyVarBind ), ( "x", VarBind <| TyVar 0 1 ) ]
                            , term = TmConst I TmTrue
                            , ty = TyName "Bool"
                            }
                        , top =
                            { ctx = [ ( "X", TyVarBind ), ( "x", VarBind <| TyVar 0 1 ) ]
                            , term = TmConst I TmTrue
                            , ty = TyName "Bool"
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TFalse" <|
            \_ ->
                checkRule
                    (TFalse
                        { bottom =
                            { ctx = [ ( "X", TyVarBind ), ( "x", VarBind <| TyVar 0 1 ) ]
                            , term = TmConst I TmFalse
                            , ty = TyName "Bool"
                            }
                        , top =
                            { ctx = [ ( "X", TyVarBind ), ( "x", VarBind <| TyVar 0 1 ) ]
                            , term = TmConst I TmFalse
                            , ty = TyName "Bool"
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TAbs" <|
            \_ ->
                checkRule
                    (TAbs
                        { bottom =
                            { ctx = []
                            , term = TmAbs I "x" (TyName "Bool") (TmVar I 0 0)
                            , ty = TyArr (TyName "Bool") (TyName "Bool")
                            }
                        , top =
                            { ctx = [ ( "x", VarBind <| TyName "Bool" ) ]
                            , term = TmVar I 0 0
                            , ty = TyName "Bool"
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TApp" <|
            \_ ->
                checkRule
                    (TApp
                        { bottom =
                            { ctx = []
                            , term = TmApp I (TmAbs I "x" (TyName "Bool") (TmVar I 0 0)) (TmConst I TmTrue)
                            , ty = TyName "Bool"
                            }
                        , top1 =
                            { ctx = []
                            , term = TmAbs I "x" (TyName "Bool") (TmVar I 0 0)
                            , ty = TyArr (TyName "Bool") (TyName "Bool")
                            }
                        , top2 =
                            { ctx = []
                            , term = TmConst I TmTrue
                            , ty = TyName "Bool"
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TTAbs {TypeVar1, TypeVar2} |- ΛTypeVar3. λtermVar3: TypeVar3. termVar3" <|
            \_ ->
                checkRule
                    (TTAbs
                        { bottom =
                            { ctx =
                                [ ( "TypeVar2", TyVarBind )
                                , ( "TypeVar1", TyVarBind )
                                ]
                            , term =
                                TmTAbs I
                                    "TypeVar3"
                                    (TmAbs I "termVar3" (TyVar 0 3) (TmVar I 0 4))
                            , ty = TyAll "TypeVar3" <| TyArr (TyVar 0 3) (TyVar 0 3)
                            }
                        , top =
                            { ctx =
                                [ ( "TypeVar3", TyVarBind )
                                , ( "TypeVar2", TyVarBind )
                                , ( "TypeVar1", TyVarBind )
                                ]
                            , term = TmAbs I "termVar3" (TyVar 0 3) (TmVar I 0 4)
                            , ty = TyArr (TyVar 0 3) (TyVar 0 3)
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TTApp {TypeVar1, TypeVar2} |- (ΛTypeVar3. λtermVar3: TypeVar3. termVar3) [TypeVar1->TypeVar2]" <|
            \_ ->
                checkRule
                    (TTApp
                        { bottom =
                            { ctx =
                                [ ( "TypeVar2", TyVarBind )
                                , ( "TypeVar1", TyVarBind )
                                ]
                            , term =
                                TmTApp I
                                    (TmTAbs I
                                        "TypeVar3"
                                        (TmAbs I "termVar3" (TyVar 0 3) (TmVar I 0 4))
                                    )
                                    (TyArr (TyVar 1 2) (TyVar 0 2))
                            , ty = TyArr (TyArr (TyVar 1 2) (TyVar 0 2)) (TyArr (TyVar 1 2) (TyVar 0 2))
                            }
                        , top =
                            { ctx =
                                [ ( "TypeVar2", TyVarBind )
                                , ( "TypeVar1", TyVarBind )
                                ]
                            , term =
                                TmTAbs I
                                    "TypeVar3"
                                    (TmAbs I "termVar3" (TyVar 0 3) (TmVar I 0 4))
                            , ty = TyAll "Alpha" <| TyArr (TyVar 0 3) (TyVar 0 3)
                            }
                        }
                    )
                    |> Expect.equal True
        ]
