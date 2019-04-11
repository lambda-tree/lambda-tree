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
                            { ctx = [ ( "x", VarBind <| TyConst TyBool ) ]
                            , term = TmVar I 0 1
                            , ty = TyConst TyBool
                            }
                        , top =
                            { ctx = [ ( "x", VarBind <| TyConst TyBool ) ]
                            , term = TmVar I 0 1
                            , ty = TyConst TyBool
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TIf" <|
            \_ ->
                checkRule
                    (TIf
                        { bottom =
                            { ctx = [ ( "X", TyVarBind ), ( "y", VarBind <| TyConst TyBool ), ( "x", VarBind <| TyVar 1 2 ) ]
                            , term = TmIf I (TmVar I 1 3) (TmVar I 0 3) (TmApp I (TmAbs I "x" (Just <| TyConst TyBool) (TmVar I 1 4)) (TmConst I TmTrue))
                            , ty = TyVar 2 3
                            }
                        , top1 =
                            { ctx = [ ( "X", TyVarBind ), ( "y", VarBind <| TyConst TyBool ), ( "x", VarBind <| TyVar 1 2 ) ]
                            , term = TmVar I 1 3
                            , ty = TyConst TyBool
                            }
                        , top2 =
                            { ctx = [ ( "X", TyVarBind ), ( "y", VarBind <| TyConst TyBool ), ( "x", VarBind <| TyVar 1 2 ) ]
                            , term = TmVar I 0 3
                            , ty = TyVar 2 3
                            }
                        , top3 =
                            { ctx = [ ( "X", TyVarBind ), ( "y", VarBind <| TyConst TyBool ), ( "x", VarBind <| TyVar 1 2 ) ]
                            , term = TmApp I (TmAbs I "x" (Just <| TyConst TyBool) (TmVar I 1 4)) (TmConst I TmTrue)
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
                            , ty = TyConst TyBool
                            }
                        , top =
                            { ctx = [ ( "X", TyVarBind ), ( "x", VarBind <| TyVar 0 1 ) ]
                            , term = TmConst I TmTrue
                            , ty = TyConst TyBool
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
                            , ty = TyConst TyBool
                            }
                        , top =
                            { ctx = [ ( "X", TyVarBind ), ( "x", VarBind <| TyVar 0 1 ) ]
                            , term = TmConst I TmFalse
                            , ty = TyConst TyBool
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
                            , term = TmAbs I "x" (Just <| TyConst TyBool) (TmVar I 0 0)
                            , ty = TyArr (TyConst TyBool) (TyConst TyBool)
                            }
                        , top =
                            { ctx = [ ( "x", VarBind <| TyConst TyBool ) ]
                            , term = TmVar I 0 0
                            , ty = TyConst TyBool
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
                            , term = TmApp I (TmAbs I "x" (Just <| TyConst TyBool) (TmVar I 0 0)) (TmConst I TmTrue)
                            , ty = TyConst TyBool
                            }
                        , top1 =
                            { ctx = []
                            , term = TmAbs I "x" (Just <| TyConst TyBool) (TmVar I 0 0)
                            , ty = TyArr (TyConst TyBool) (TyConst TyBool)
                            }
                        , top2 =
                            { ctx = []
                            , term = TmConst I TmTrue
                            , ty = TyConst TyBool
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
                                    (TmAbs I "termVar3" (Just <| TyVar 0 3) (TmVar I 0 4))
                            , ty = TyAll "TypeVar3" <| TyArr (TyVar 0 3) (TyVar 0 3)
                            }
                        , top =
                            { ctx =
                                [ ( "TypeVar3", TyVarBind )
                                , ( "TypeVar2", TyVarBind )
                                , ( "TypeVar1", TyVarBind )
                                ]
                            , term = TmAbs I "termVar3" (Just <| TyVar 0 3) (TmVar I 0 4)
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
                                        (TmAbs I "termVar3" (Just <| TyVar 0 3) (TmVar I 0 4))
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
                                    (TmAbs I "termVar3" (Just <| TyVar 0 3) (TmVar I 0 4))
                            , ty = TyAll "Alpha" <| TyArr (TyVar 0 3) (TyVar 0 3)
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TGen {} |- (λtermVar1. termVar1) : Forall TypeVar1. TypeVar1 -> TypeVar1" <|
            \_ ->
                checkRule
                    (TGen
                        { bottom =
                            { ctx = []
                            , term = TmAbs I "termVar1" Nothing (TmVar I 0 1)
                            , ty = TyAll "TypeVar1" <| TyArr (TyVar 0 1) (TyVar 0 1)
                            }
                        , top =
                            { ctx = []
                            , term = TmAbs I "termVar1" Nothing (TmVar I 0 1)
                            , ty = TyArr (TyName "TypeVar1") (TyName "TypeVar1")
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TInst {} |- (λtermVar1. termVar1) : Bool -> Bool" <|
            \_ ->
                checkRule
                    (TInst
                        { bottom =
                            { ctx = []
                            , term = TmAbs I "termVar1" Nothing (TmVar I 0 1)
                            , ty = TyArr (TyConst TyBool) (TyConst TyBool)
                            }
                        , top =
                            { ctx = []
                            , term = TmAbs I "termVar1" Nothing (TmVar I 0 1)
                            , ty = TyAll "TypeVar1" <| TyArr (TyVar 0 1) (TyVar 0 1)
                            }
                        }
                    )
                    |> Expect.equal True
        , test "TInst {} |- (λtermVar1. termVar1) : A -> A" <|
            \_ ->
                checkRule
                    (TInst
                        { bottom =
                            { ctx = []
                            , term = TmAbs I "termVar1" Nothing (TmVar I 0 1)
                            , ty = TyArr (TyName "A") (TyName "A")
                            }
                        , top =
                            { ctx = []
                            , term = TmAbs I "termVar1" Nothing (TmVar I 0 1)
                            , ty = TyAll "A" <| TyArr (TyVar 0 1) (TyVar 0 1)
                            }
                        }
                    )
                    |> Expect.equal True
        ]
