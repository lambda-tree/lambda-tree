module Lambda.ExpressionUtilsTests exposing (..)

import Expect exposing (Expectation)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (..)
import Set
import Test exposing (..)


typeShiftAboveTest : Test
typeShiftAboveTest =
    describe "typeShiftAbove"
        [ test "Shifting Type" <|
            \_ ->
                typeShiftAbove -1 0 (TyAll "alpha" (TyArr (TyVar 1 1) (TyVar 1 1)))
                    |> Expect.equal (TyAll "alpha" (TyArr (TyVar 0 0) (TyVar 0 0)))
        ]


tytermSubstTopTest : Test
tytermSubstTopTest =
    describe "tytermSubstTop"
        [ test "T-Inst Step - System F" <|
            \_ ->
                tytermSubstTop (TyConst TyBool) (TmAbs I "a" (Just <| TyVar 0 1) <| TmVar I 0 2)
                    |> Expect.equal (TmAbs I "a" (Just <| TyConst TyBool) <| TmVar I 0 1)
        ]


termShiftTest : Test
termShiftTest =
    describe "termShift"
        [ test "Should shift free variable" <|
            \_ ->
                termShift 2 (TmVar I 1 3)
                    |> Expect.equal (TmVar I 3 5)
        , test "Should not shift bound variable" <|
            \_ ->
                termShift 2
                    (TmAbs I
                        "x"
                        (Just <| TyArr (TyConst TyBool) (TyConst TyBool))
                        (TmVar I 0 2)
                    )
                    |> Expect.equal
                        (TmAbs I
                            "x"
                            (Just <| TyArr (TyConst TyBool) (TyConst TyBool))
                            (TmVar I 0 4)
                        )
        , test "Should shift only free variable and not shift bound." <|
            \_ ->
                termShift 5
                    (TmApp I
                        (TmAbs I
                            "x"
                            (Just <| TyArr (TyConst TyBool) (TyConst TyBool))
                            (TmVar I 0 2)
                        )
                        (TmVar I 1 2)
                    )
                    |> Expect.equal
                        (TmApp I
                            (TmAbs I
                                "x"
                                (Just <| TyArr (TyConst TyBool) (TyConst TyBool))
                                (TmVar I 0 7)
                            )
                            (TmVar I 6 7)
                        )
        ]


termShiftAboveTest : Test
termShiftAboveTest =
    describe "termShiftAbove"
        [ test "Should not shift bound variable and variable that is 'bound' for this case, i.e. has index lower than ctx length " <|
            \_ ->
                termShiftAbove 5
                    2
                    (TmApp I
                        (TmAbs I
                            "x"
                            (Just <| TyArr (TyConst TyBool) (TyConst TyBool))
                            (TmVar I 0 2)
                        )
                        (TmVar I 1 2)
                    )
                    |> Expect.equal
                        (TmApp I
                            (TmAbs I
                                "x"
                                (Just <| TyArr (TyConst TyBool) (TyConst TyBool))
                                (TmVar I 0 7)
                            )
                            (TmVar I 1 7)
                        )
        ]


termSubstTest : Test
termSubstTest =
    describe "termSubst"
        [ test "Should not shift bound variable and variable that is 'bound' for this case, i.e. has index lower than ctx length" <|
            \_ ->
                termShiftAbove 5
                    2
                    (TmApp I
                        (TmAbs I
                            "x"
                            (Just <| TyArr (TyConst TyBool) (TyConst TyBool))
                            (TmVar I 0 2)
                        )
                        (TmVar I 1 2)
                    )
                    |> Expect.equal
                        (TmApp I
                            (TmAbs I
                                "x"
                                (Just <| TyArr (TyConst TyBool) (TyConst TyBool))
                                (TmVar I 0 7)
                            )
                            (TmVar I 1 7)
                        )
        ]


equalTypesTest : Test
equalTypesTest =
    describe "equalTypes"
        [ test "Should be true if type names are same" <|
            \_ ->
                equalTypes emptycontext (TyName "B") emptycontext (TyName "B")
                    |> Expect.equal
                        True
        , test "Should be false if type names are not same" <|
            \_ ->
                equalTypes emptycontext (TyName "B") emptycontext (TyName "")
                    |> Expect.equal
                        False
        , test "Should be true if type constants are same" <|
            \_ ->
                equalTypes emptycontext (TyConst TyBool) emptycontext (TyConst TyBool)
                    |> Expect.equal
                        True
        , test "Should be false if type constants are not same" <|
            \_ ->
                equalTypes emptycontext (TyConst TyBool) emptycontext (TyConst TyInt)
                    |> Expect.equal
                        False
        , test "Should be true if forall names are same" <|
            \_ ->
                equalTypes emptycontext (TyAll "x" <| TyVar 0 1) emptycontext (TyAll "x" <| TyVar 0 1)
                    |> Expect.equal
                        True
        , test "Should be false if forall names are not same" <|
            \_ ->
                equalTypes emptycontext (TyAll "x" <| TyVar 0 1) emptycontext (TyAll "y" <| TyVar 0 1)
                    |> Expect.equal
                        False
        , test "Should be true if both TyArr types are same" <|
            \_ ->
                equalTypes
                    emptycontext
                    (TyArr (TyConst TyBool) (TyConst TyInt))
                    emptycontext
                    (TyArr (TyConst TyBool) (TyConst TyInt))
                    |> Expect.equal
                        True
        , test "Should be false if left TyArr type is different" <|
            \_ ->
                equalTypes
                    emptycontext
                    (TyArr (TyConst TyInt) (TyConst TyInt))
                    emptycontext
                    (TyArr (TyConst TyBool) (TyConst TyInt))
                    |> Expect.equal
                        False
        , test "Should be false if right TyArr type is different" <|
            \_ ->
                equalTypes
                    emptycontext
                    (TyArr (TyConst TyBool) (TyConst TyBool))
                    emptycontext
                    (TyArr (TyConst TyBool) (TyConst TyInt))
                    |> Expect.equal
                        False
        , test "Should be true if ctxts and vars are same and variables exist" <|
            \_ ->
                equalTypes
                    [ ( "x", TyVarBind ) ]
                    (TyVar 0 1)
                    [ ( "x", TyVarBind ) ]
                    (TyVar 0 1)
                    |> Expect.equal
                        True
        , test "Should be true even if variables don't exist" <|
            \_ ->
                equalTypes
                    emptycontext
                    (TyVar 0 1)
                    emptycontext
                    (TyVar 0 1)
                    |> Expect.equal
                        True
        , test "Should be false if ctxt var names are different" <|
            \_ ->
                equalTypes
                    [ ( "x", TyVarBind ) ]
                    (TyVar 0 1)
                    [ ( "y", TyVarBind ) ]
                    (TyVar 0 1)
                    |> Expect.equal
                        False
        , test "Should be true if variables are referencing from different positions of context" <|
            \_ ->
                equalTypes [ ( "x", VarBind (TyVar 0 1) ), ( "x", TyVarBind ) ] (TyVar 1 2) [ ( "x", TyVarBind ) ] (TyVar 0 1)
                    |> Expect.equal
                        True
        , test "Should be true if variables are referencing from different positions of context 2" <|
            \_ ->
                equalTypes
                    [ ( "X", TyVarBind )
                    , ( "Y", TyVarBind )
                    ]
                    (TyAll "A" <| TyArr (TyVar 0 3) (TyVar 1 3))
                    [ ( "Z", TyVarBind )
                    , ( "x", VarBind <| TyAll "A" <| TyArr (TyVar 0 3) (TyVar 1 3) )
                    , ( "X", TyVarBind )
                    , ( "Y", TyVarBind )
                    ]
                    (TyAll "A" <| TyArr (TyVar 0 5) (TyVar 3 5))
                    |> Expect.equal
                        True
        ]


degeneralizeTypeTopTest : Test
degeneralizeTypeTopTest =
    describe "degeneralizeTypeTop"
        [ test "Should degeneralize empty context single var type" <|
            \_ ->
                degeneralizeTypeTop [] (TyAll "TyVar1" <| TyVar 0 1)
                    |> Expect.equal (TyName "TyVar1")
        , test "Should degeneralize full context single var type" <|
            \_ ->
                degeneralizeTypeTop
                    [ ( "X", TyVarBind )
                    , ( "y", VarBind <| TyVar 0 1 )
                    , ( "Z", TyVarBind )
                    ]
                    (TyAll "TyVar1" <| TyVar 0 4)
                    |> Expect.equal (TyName "TyVar1")
        , test "Should degeneralize full context multiple var type" <|
            \_ ->
                degeneralizeTypeTop
                    [ ( "X", TyVarBind )
                    , ( "y", VarBind <| TyVar 0 1 )
                    , ( "Z", TyVarBind )
                    ]
                    (TyAll "TyVar1" <| TyArr (TyVar 1 4) (TyArr (TyConst TyBool) (TyVar 0 4)))
                    |> Expect.equal (TyArr (TyVar 0 3) (TyArr (TyConst TyBool) (TyName "TyVar1")))
        , test "Should degeneralize full context multiple vars bound and free type" <|
            \_ ->
                degeneralizeTypeTop
                    [ ( "X", TyVarBind )
                    , ( "y", VarBind <| TyVar 0 1 )
                    , ( "Z", TyVarBind )
                    ]
                    (TyAll "TyVar1" <|
                        TyArr
                            (TyVar 1 4)
                            (TyArr
                                (TyAll "TyVar2" <| TyArr (TyConst TyBool) (TyVar 0 5))
                                (TyVar 0 4)
                            )
                    )
                    |> Expect.equal
                        (TyArr
                            (TyVar 0 3)
                            (TyArr
                                (TyAll "TyVar2" <| TyArr (TyConst TyBool) (TyVar 0 4))
                                (TyName "TyVar1")
                            )
                        )
        ]


degeneralizeTermTopTest : Test
degeneralizeTermTopTest =
    describe "degeneralizeTermTop"
        [ test "Should degeneralize term top level if there is single type abstraction" <|
            \_ ->
                degeneralizeTermTop []
                    (TmTAbs I "A" <|
                        TmAbs I "f" (Just <| TyArr (TyVar 0 1) <| TyVar 0 1) <|
                            TmAbs I "x" (Just <| TyVar 1 2) <|
                                (TmApp I (TmVar I 1 3) <| TmApp I (TmVar I 1 3) <| TmVar I 0 3)
                    )
                    |> Expect.equal
                        (TmAbs I "f" (Just <| TyArr (TyName "A") <| TyName "A") <|
                            TmAbs I "x" (Just <| TyName "A") <|
                                (TmApp I (TmVar I 1 2) <| TmApp I (TmVar I 1 2) <| TmVar I 0 2)
                        )
        , test "Should degeneralize term top level if there are 2 type abstractions" <|
            \_ ->
                degeneralizeTermTop []
                    (TmTAbs I "A" <|
                        TmTAbs I "B" <|
                            (TmAbs I "termVar1" (Just <| TyVar 1 2) <| TmAbs I "termVar2" (Just <| TyVar 1 3) <| TmVar I 1 4)
                    )
                    |> Expect.equal
                        (TmTAbs I "B" <|
                            (TmAbs I "termVar1" (Just <| TyName "A") <| TmAbs I "termVar2" (Just <| TyVar 1 2) <| TmVar I 1 3)
                        )
        , test "Should degeneralize term top level if there is already degeneralized abstraction" <|
            \_ ->
                degeneralizeTermTop []
                    (TmTAbs I "B" <|
                        (TmAbs I "termVar1" (Just <| TyName "A") <| TmAbs I "termVar2" (Just <| TyVar 1 2) <| TmVar I 1 3)
                    )
                    |> Expect.equal
                        (TmAbs I "termVar1" (Just <| TyName "A") <| TmAbs I "termVar2" (Just <| TyName "B") <| TmVar I 1 2)
        ]


generalizeTypeTopTest : Test
generalizeTypeTopTest =
    describe "generalizeTypeTop"
        [ test "Should generalize empty context single var type" <|
            \_ ->
                generalizeTypeTop [] (TyName "TyVar1") "TyVar1"
                    |> Expect.equal (TyAll "TyVar1" <| TyVar 0 1)
        , test "Should generalize full context single var type" <|
            \_ ->
                generalizeTypeTop
                    [ ( "X", TyVarBind )
                    , ( "y", VarBind <| TyVar 0 1 )
                    , ( "Z", TyVarBind )
                    ]
                    (TyName "TyVar1")
                    "TyVar1"
                    |> Expect.equal (TyAll "TyVar1" <| TyVar 0 4)
        , test "Should generalize full context multiple var type" <|
            \_ ->
                generalizeTypeTop
                    [ ( "X", TyVarBind )
                    , ( "y", VarBind <| TyVar 0 1 )
                    , ( "Z", TyVarBind )
                    ]
                    (TyArr (TyVar 0 3) (TyArr (TyConst TyBool) (TyName "TyVar1")))
                    "TyVar1"
                    |> Expect.equal
                        (TyAll "TyVar1" <| TyArr (TyVar 1 4) (TyArr (TyConst TyBool) (TyVar 0 4)))
        , test "Should generalize full context multiple vars bound and free type" <|
            \_ ->
                generalizeTypeTop
                    [ ( "X", TyVarBind )
                    , ( "y", VarBind <| TyVar 0 1 )
                    , ( "Z", TyVarBind )
                    ]
                    (TyArr
                        (TyVar 0 3)
                        (TyArr
                            (TyAll "TyVar2" <| TyArr (TyConst TyBool) (TyVar 0 4))
                            (TyName "TyVar1")
                        )
                    )
                    "TyVar1"
                    |> Expect.equal
                        (TyAll "TyVar1" <|
                            TyArr
                                (TyVar 1 4)
                                (TyArr
                                    (TyAll "TyVar2" <| TyArr (TyConst TyBool) (TyVar 0 5))
                                    (TyVar 0 4)
                                )
                        )
        ]


degeneralizeTypeTest : Test
degeneralizeTypeTest =
    describe "degeneralizeType"
        [ test "Should degeneralize empty context single var type" <|
            \_ ->
                degeneralizeType [] (TyAll "TyVar1" <| TyVar 0 1)
                    |> Expect.equal (TyName "TyVar1")
        , test "Should degeneralize full context single var type" <|
            \_ ->
                degeneralizeType
                    [ ( "X", TyVarBind )
                    , ( "y", VarBind <| TyVar 0 1 )
                    , ( "Z", TyVarBind )
                    ]
                    (TyAll "TyVar1" <| TyVar 0 4)
                    |> Expect.equal (TyName "TyVar1")
        , test "Should degeneralize full context multiple var type" <|
            \_ ->
                degeneralizeType
                    [ ( "X", TyVarBind )
                    , ( "y", VarBind <| TyVar 0 1 )
                    , ( "Z", TyVarBind )
                    ]
                    (TyAll "TyVar1" <| TyArr (TyVar 1 4) (TyArr (TyConst TyBool) (TyVar 0 4)))
                    |> Expect.equal (TyArr (TyVar 0 3) (TyArr (TyConst TyBool) (TyName "TyVar1")))
        , test "Should degeneralize full context multiple bound vars" <|
            \_ ->
                degeneralizeType
                    [ ( "X", TyVarBind )
                    , ( "y", VarBind <| TyVar 0 1 )
                    , ( "Z", TyVarBind )
                    ]
                    (TyAll "TyVar1" <|
                        TyAll "TyVar2" <|
                            TyAll "TyVar3" <|
                                TyArr (TyVar 3 6) (TyArr (TyVar 2 6) (TyVar 0 6))
                    )
                    |> Expect.equal (TyArr (TyVar 0 3) (TyArr (TyName "TyVar1") (TyName "TyVar3")))
        ]


ftvTyTest : Test
ftvTyTest =
    describe "ftvTy"
        [ test "Should return free type variables of type" <|
            \_ ->
                ftvTy
                    (TyAll "TyVar1" <|
                        TyArr
                            (TyVar 0 1)
                            (TyArr
                                (TyName "A")
                                (TyArr (TyName "B") (TyName "C"))
                            )
                    )
                    |> Expect.equal (Set.fromList [ "A", "B", "C" ])
        ]


ftvCtxTest : Test
ftvCtxTest =
    describe "ftvCtx"
        [ test "Should return free type variables of ctx" <|
            \_ ->
                ftvCtx
                    [ ( "TyVarX", TyVarBind )
                    , ( "termVar1", VarBind <| TyVar 0 1 )
                    , ( "termVar2", VarBind <| TyName "X" )
                    , ( "termVar3"
                      , VarBind <|
                            (TyAll "TyVar1" <|
                                TyArr
                                    (TyVar 0 1)
                                    (TyArr
                                        (TyName "A")
                                        (TyArr (TyName "B") (TyName "C"))
                                    )
                            )
                      )
                    ]
                    |> Expect.equal (Set.fromList [ "A", "B", "C", "TyVarX", "X" ])
        ]


substTest : Test
substTest =
    describe "subst"
        [ test "Should substitute tyS for the free variable in simple case" <|
            \_ ->
                substFtv
                    [ ( TyArr
                            (TyName "Y")
                            (TyName "Z")
                      , "X"
                      )
                    ]
                    (TyName "X")
                    |> Expect.equal (TyArr (TyName "Y") (TyName "Z"))
        , test "Should substitute tyS for the free variable in TyArr" <|
            \_ ->
                substFtv
                    [ ( TyArr (TyName "Y") (TyName "Z"), "X" ) ]
                    (TyArr (TyName "X") (TyName "X"))
                    |> Expect.equal
                        (TyArr
                            (TyArr
                                (TyName "Y")
                                (TyName "Z")
                            )
                            (TyArr
                                (TyName "Y")
                                (TyName "Z")
                            )
                        )
        ]


unifyTypeTest : Test
unifyTypeTest =
    describe "unifyType"
        [ test "Should unify types 1" <|
            \_ ->
                unifyType
                    (TyName "X")
                    (TyName "Y")
                    |> Expect.equal (Ok [ ( TyName "Y", "X" ) ])
        , test "Should unify types 2" <|
            \_ ->
                unifyType
                    (TyArr (TyName "X") (TyName "X"))
                    (TyArr (TyConst TyBool) (TyConst TyBool))
                    |> Expect.equal (Ok [ ( TyConst TyBool, "X" ) ])
        , test "Should unify types 3" <|
            \_ ->
                unifyType
                    (TyArr (TyName "X") (TyArr (TyName "Y") (TyName "X")))
                    (TyArr (TyConst TyBool) (TyArr (TyName "Z") (TyConst TyBool)))
                    |> Expect.equal (Ok [ ( TyName "Z", "Y" ), ( TyConst TyBool, "X" ) ])
        , test "Should unify types 4" <|
            \_ ->
                unifyType
                    (TyArr (TyConst TyBool) (TyArr (TyName "Y") (TyConst TyBool)))
                    (TyArr (TyConst TyBool) (TyArr (TyName "Z") (TyConst TyBool)))
                    |> Expect.equal (Ok [ ( TyName "Z", "Y" ) ])
        , test "Should unify types 5" <|
            \_ ->
                unifyType
                    (TyArr (TyConst TyBool) (TyArr (TyConst TyBool) (TyConst TyBool)))
                    (TyArr (TyConst TyBool) (TyArr (TyConst TyBool) (TyConst TyBool)))
                    |> Expect.equal (Ok [])
        , test "Should unify types 6" <|
            \_ ->
                unifyType
                    (TyArr (TyConst TyBool) (TyConst TyBool))
                    (TyArr (TyName "A") (TyName "A"))
                    |> Expect.equal (Ok [ ( TyConst TyBool, "A" ) ])
        , test "Should unify types 7" <|
            \_ ->
                unifyType
                    (TyArr (TyName "A") (TyName "A"))
                    (TyArr (TyConst TyBool) (TyConst TyBool))
                    |> Expect.equal (Ok [ ( TyConst TyBool, "A" ) ])
        ]


freshVarNameTest : Test
freshVarNameTest =
    describe "freshVarName"
        [ test "Should return same var if not in free vars" <|
            \_ ->
                freshVarName
                    (Set.fromList [ "Y" ])
                    "X"
                    |> Expect.equal "X"
        , test "Should return fresh counted var if in free vars 1" <|
            \_ ->
                freshVarName
                    (Set.fromList [ "Y", "X" ])
                    "X"
                    |> Expect.equal "X1"
        , test "Should return fresh counted var if in free vars 2" <|
            \_ ->
                freshVarName
                    (Set.fromList [ "Y", "X", "X1", "X2", "X3" ])
                    "X"
                    |> Expect.equal "X4"
        ]


renameBoundVarsWithFreshTest : Test
renameBoundVarsWithFreshTest =
    describe "renameBoundVarsWithFresh"
        [ test "Should return same type if not in free vars" <|
            \_ ->
                renameBoundVarsWithFresh
                    (Set.fromList [ "Y" ])
                    (TyAll "X" <| TyVar 0 1)
                    |> Expect.equal (TyAll "X" <| TyVar 0 1)
        , test "Should return fresh counted var if in free vars 1" <|
            \_ ->
                renameBoundVarsWithFresh
                    (Set.fromList [ "Y", "X" ])
                    (TyAll "X" <| TyVar 0 1)
                    |> Expect.equal (TyAll "X1" <| TyVar 0 1)
        , test "Should return fresh counted var if in free vars 2" <|
            \_ ->
                renameBoundVarsWithFresh
                    (Set.fromList [ "Y", "X", "X1", "X2", "X3" ])
                    (TyAll "X" <| TyVar 0 1)
                    |> Expect.equal (TyAll "X4" <| TyVar 0 1)
        , test "Should return fresh counted var if in free vars multiple vars 1" <|
            \_ ->
                renameBoundVarsWithFresh
                    (Set.fromList [ "Y", "X", "X1", "X2", "X3" ])
                    (TyAll "X" <| TyAll "Z" <| TyAll "Y" <| TyArr (TyVar 0 3) (TyVar 2 3))
                    |> Expect.equal (TyAll "X4" <| TyAll "Z" <| TyAll "Y1" <| TyArr (TyVar 0 3) (TyVar 2 3))
        ]


isSpecializedTypeTest : Test
isSpecializedTypeTest =
    describe "isSpecializedType"
        [ test "Should be true if both equally generic" <|
            \_ ->
                isSpecializedType []
                    (TyAll "X" <| TyArr (TyVar 0 1) (TyVar 0 1))
                    (TyAll "Y" <| TyArr (TyVar 0 1) (TyVar 0 1))
                    |> Expect.equal (Ok <| True)
        , test "Should be true if second is more strictly specialized 1" <|
            \_ ->
                isSpecializedType []
                    (TyAll "X" <| TyArr (TyVar 0 1) (TyVar 0 1))
                    (TyArr (TyConst TyBool) (TyConst TyBool))
                    |> Expect.equal (Ok <| True)
        , test "Should be false if first is more strictly specialized" <|
            \_ ->
                isSpecializedType []
                    (TyArr (TyConst TyBool) (TyConst TyBool))
                    (TyAll "X" <| TyArr (TyVar 0 1) (TyVar 0 1))
                    |> Expect.equal (Ok <| False)
        , test "Should be true if second is more strictly specialized 2" <|
            \_ ->
                isSpecializedType []
                    (TyAll "X" <| TyArr (TyVar 0 1) (TyVar 0 1))
                    (TyAll "X" <|
                        TyAll "Y" <|
                            TyArr
                                (TyArr (TyVar 0 1) (TyVar 0 1))
                                (TyArr (TyVar 0 1) (TyVar 0 1))
                    )
                    |> Expect.equal (Ok <| True)
        , test "Should be true if second is more strictly specialized 3" <|
            \_ ->
                isSpecializedType []
                    (TyAll "X" <| TyArr (TyVar 0 1) (TyVar 0 1))
                    (TyArr (TyName "X") (TyName "X"))
                    |> Expect.equal (Ok <| True)
        , test "Should be false if substitution must be into a free variable" <|
            \_ ->
                isSpecializedType []
                    (TyArr (TyName "X") (TyName "X"))
                    (TyAll "X" <| TyArr (TyVar 0 1) (TyVar 0 1))
                    |> Expect.equal (Ok <| False)
        , test "Should be false if substituting to non bound variable" <|
            \_ ->
                isSpecializedType []
                    (TyAll "X" <| TyArr (TyName "Y") (TyName "Y"))
                    (TyArr (TyName "X") (TyName "X"))
                    |> Expect.equal (Ok <| False)
        ]


instTest : Test
instTest =
    describe "inst"
        [ test "should inst vars" <|
            \_ ->
                inst [] (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1)
                    |> Expect.equal (TyArr (TyName "A") <| TyName "A")
        , test "should inst vars with fresh variables" <|
            \_ ->
                inst [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" ) ] (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1)
                    |> Expect.equal (TyArr (TyName "A1") <| TyName "A1")
        ]


genTest : Test
genTest =
    describe "gen"
        [ test "should gen free vars" <|
            \_ ->
                gen [] (TyArr (TyName "A") <| TyArr (TyName "B") <| TyName "A")
                    |> Expect.equal (TyAll "A" <| TyAll "B" <| TyArr (TyVar 1 2) <| TyArr (TyVar 0 2) <| TyVar 1 2)
        , test "should gen vars with non empty ctx if they're not free in ctx" <|
            \_ ->
                gen [ ( "x", VarBind <| TyName "C" ) ] (TyArr (TyName "A") <| TyArr (TyName "B") <| TyName "A")
                    |> Expect.equal (TyAll "A" <| TyAll "B" <| TyArr (TyVar 1 3) <| TyArr (TyVar 0 3) <| TyVar 1 3)
        , test "should not gen free vars in ctx" <|
            \_ ->
                gen [ ( "x", VarBind <| TyName "A" ) ] (TyArr (TyName "A") <| TyArr (TyName "B") <| TyName "A")
                    |> Expect.equal (TyAll "B" <| TyArr (TyName "A") <| TyArr (TyVar 0 2) <| TyName "A")
        ]


wTest : Test
wTest =
    describe "w"
        [ describe "TmVar"
            [ test "generic variable should be instantiated with fresh type variables" <|
                \_ ->
                    w
                        [ ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) ) ]
                        (TmVar I 0 1)
                        |> Expect.equal (Ok <| ( [], TyArr (TyName "A") <| TyName "A" ))
            , test "generic variable should be instantiated with fresh type variables 2" <|
                \_ ->
                    w
                        [ ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        , ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        ]
                        (TmVar I 0 2)
                        |> Expect.equal (Ok <| ( [], TyArr (TyName "A1") <| TyName "A1" ))
            , test "generic variable should be instantiated with fresh type variables 3" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        , ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        ]
                        (TmVar I 1 2)
                        |> Expect.equal (Ok <| ( [], TyArr (TyName "A1") <| TyName "A1" ))
            ]
        , describe "TmConst"
            [ test "true : Bool" <|
                \_ ->
                    w
                        [ ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) ) ]
                        (TmConst I TmTrue)
                        |> Expect.equal (Ok <| ( [], TyConst TyBool ))
            , test "false : Bool" <|
                \_ ->
                    w
                        [ ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) ) ]
                        (TmConst I TmFalse)
                        |> Expect.equal (Ok <| ( [], TyConst TyBool ))
            ]
        , describe "TmAbs"
            [ test "lambda a. a : X -> X" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        , ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        ]
                        (TmAbs I "termVar1" Nothing <| TmVar I 0 3)
                        |> Expect.equal (Ok <| ( [], TyArr (TyName "X") <| TyName "X" ))
            , test "lambda a. x : X -> A -> A" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        , ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        ]
                        (TmAbs I "termVar1" Nothing <| TmVar I 1 3)
                        |> Expect.equal (Ok <| ( [], TyArr (TyName "X") <| TyArr (TyName "A") <| TyName "A" ))
            , test "lambda a : X. x : X -> A -> A" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        , ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        ]
                        (TmAbs I "termVar1" (Just <| TyName "X") <| TmVar I 1 3)
                        |> Expect.equal (Ok <| ( [], TyArr (TyName "X") <| TyArr (TyName "A") <| TyName "A" ))
            , test "lambda a : Y. x : X -> A -> A" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        , ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        ]
                        (TmAbs I "termVar1" (Just <| TyName "Y") <| TmVar I 1 3)
                        |> Expect.equal (Ok <| ( [], TyArr (TyName "Y") <| TyArr (TyName "A") <| TyName "A" ))
            , test "lambda a : Y -> Z. x : (Y -> Z) -> A -> A" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        , ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        ]
                        (TmAbs I "termVar1" (Just <| TyArr (TyName "Y") <| TyName "Z") <| TmVar I 1 3)
                        |> Expect.equal (Ok <| ( [], TyArr (TyArr (TyName "Y") <| TyName "Z") <| TyArr (TyName "A") <| TyName "A" ))
            ]
        , describe "TmApp"
            [ test "(lambda termVar1. termVar1) x : A -> A" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        , ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        ]
                        (TmApp I (TmAbs I "termVar1" Nothing <| TmVar I 0 3) (TmVar I 0 2))
                        |> Result.map Tuple.second
                        -- What is the substitution good for here??
                        |> Expect.equal (Ok <| TyArr (TyName "A") <| TyName "A")
            , test "(lambda termVar1. true) x : Bool" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        ]
                        (TmApp I (TmAbs I "termVar1" Nothing <| TmConst I TmTrue) (TmVar I 0 2))
                        -- What is the substitution good for here??
                        |> Result.map Tuple.second
                        |> Expect.equal (Ok <| TyConst TyBool)
            ]
        , describe "TmLet"
            [ test "Let const = lambda x. lambda y. x in const" <|
                \_ ->
                    w
                        []
                        (TmLet I
                            "const"
                            (TmAbs I "termVar1" Nothing <| TmAbs I "termVar2" Nothing <| TmVar I 1 2)
                            (TmVar I 0 1)
                        )
                        |> Result.map Tuple.second
                        -- What is the substitution good for here??
                        |> Expect.equal (Ok <| TyArr (TyName "X") <| TyArr (TyName "X1") <| TyName "X")
            ]
        ]


typeOfTest : Test
typeOfTest =
    describe "typeOf"
        [ describe "H-M"
            [ test "Let const = lambda x. lambda y. x in const : Forall X, X1. X -> X1 -> X" <|
                \_ ->
                    typeOf
                        []
                        (TmLet I
                            "const"
                            (TmAbs I "termVar1" Nothing <| TmAbs I "termVar2" Nothing <| TmVar I 1 2)
                            (TmVar I 0 1)
                        )
                        |> Expect.equal (Ok <| TyAll "X" <| TyAll "X1" <| TyArr (TyVar 1 2) <| TyArr (TyVar 0 2) <| TyVar 1 2)
            ]
        , describe "System F"
            [ test "Lambda A . lambda f: A -> A . lambda x: A. f (f x): Forall A. (A -> A) -> A -> A" <|
                \_ ->
                    typeOf
                        []
                        (TmTAbs I "A" <|
                            TmAbs I "f" (Just <| TyArr (TyVar 0 1) <| TyVar 0 1) <|
                                TmAbs I "x" (Just <| TyVar 1 2) <|
                                    (TmApp I (TmVar I 1 3) <| TmApp I (TmVar I 1 3) <| TmVar I 0 3)
                        )
                        |> Expect.equal (Ok <| TyAll "A" <| TyArr (TyArr (TyVar 0 1) <| TyVar 0 1) <| TyArr (TyVar 0 1) <| TyVar 0 1)
            , test "Lambda A. lambda termVar1: A. lambda termVar2: A. termVar1: Forall A. A -> A -> A" <|
                \_ ->
                    typeOf
                        []
                        (TmTAbs I "A" <|
                            (TmAbs I "termVar1" (Just <| TyVar 0 1) <| TmAbs I "termVar2" (Just <| TyVar 1 2) <| TmVar I 1 3)
                        )
                        |> Expect.equal (Ok <| TyAll "A" <| TyArr (TyVar 0 1) <| TyArr (TyVar 0 1) <| TyVar 0 1)
            , test "Lambda A. Lambda B. lambda termVar1: A. lambda termVar2: B. termVar1: Forall A. Forall B. A -> B -> A" <|
                \_ ->
                    typeOf
                        []
                        (TmTAbs I "A" <|
                            TmTAbs I "B" <|
                                (TmAbs I "termVar1" (Just <| TyVar 1 2) <| TmAbs I "termVar2" (Just <| TyVar 1 3) <| TmVar I 1 4)
                        )
                        |> Expect.equal (Ok <| TyAll "A" <| TyAll "B" <| TyArr (TyVar 1 2) <| TyArr (TyVar 0 2) <| TyVar 1 2)
            , test "(Lambda A. Lambda B. lambda termVar1: A. lambda termVar2: B. termVar1) [Bool]: Forall B. Bool -> B -> Bool" <|
                \_ ->
                    typeOf
                        []
                        (TmTApp I
                            (TmTAbs I "A" <|
                                TmTAbs I "B" <|
                                    (TmAbs I "termVar1" (Just <| TyVar 1 2) <| TmAbs I "termVar2" (Just <| TyVar 1 3) <| TmVar I 1 4)
                            )
                         <|
                            TyConst TyBool
                        )
                        |> Expect.equal (Ok <| TyAll "B" <| TyArr (TyConst TyBool) <| TyArr (TyVar 0 1) <| TyConst TyBool)
            ]
        ]


areHMTypesEquivalentTest : Test
areHMTypesEquivalentTest =
    describe "areHMTypesEquivalent"
        [ test "should be Ok when ty1 & ty2 are equally general and equi valently generalized" <|
            \_ ->
                areHMTypesEquivalent
                    []
                    (TyAll "A" <| TyAll "B" <| TyArr (TyVar 1 2) <| TyArr (TyVar 0 2) <| TyVar 1 2)
                    (TyAll "A" <| TyAll "B" <| TyArr (TyVar 0 2) <| TyArr (TyVar 1 2) <| TyVar 0 2)
                    |> Expect.ok
        , test "should Err when not equivalently generalized" <|
            \_ ->
                areHMTypesEquivalent
                    []
                    (TyAll "A" <| TyAll "B" <| TyArr (TyVar 1 2) <| TyArr (TyVar 0 2) <| TyVar 1 2)
                    (TyAll "Y" <| TyAll "A" <| TyArr (TyVar 0 2) <| TyArr (TyName "Z") <| TyVar 0 2)
                    |> Expect.err
        ]
