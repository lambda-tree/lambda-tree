module Lambda.ExpressionUtilsTests exposing (..)

import Expect exposing (Expectation)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (..)
import Test exposing (..)


{-| forall a. a -> a
-}
typeIdentityFunction =
    TyAll
        "alpha"
        (TyArr
            (TyVar 0 0)
            (TyVar 0 0)
        )


{-| Identity function in System F

  - In TmAbs type, the context does not yet contain the "a" variable -> so the ctx length is 1 in type,
  - In the TmAbs term, the context contains the "a" -> so the ctx length is 2
  - After substitution, the "alpha" variable is consumed, so the context length of variables is lowered by 1

-}
identityF =
    TmTAbs I "alpha" <|
        TmAbs I "a" (Just <| TyVar 0 1) <|
            TmVar I 0 2


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
                equalTypes emptycontext (TyAll "x" <| TyConst TyBool) emptycontext (TyAll "x" <| TyConst TyBool)
                    |> Expect.equal
                        True
        , test "Should be true even if forall names are not same" <|
            \_ ->
                equalTypes emptycontext (TyAll "x" <| TyConst TyBool) emptycontext (TyAll "y" <| TyConst TyBool)
                    |> Expect.equal
                        True
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
        , test "Should be false if variables don't exist" <|
            \_ ->
                equalTypes
                    emptycontext
                    (TyVar 0 1)
                    emptycontext
                    (TyVar 0 1)
                    |> Expect.equal
                        False
        , test "Should be true if ctxt var names are different" <|
            \_ ->
                equalTypes
                    [ ( "x", TyVarBind ) ]
                    (TyVar 0 1)
                    [ ( "y", TyVarBind ) ]
                    (TyVar 0 1)
                    |> Expect.equal
                        True
        , test "Should be true if variables are referencing from different positions of context" <|
            \_ ->
                equalTypes
                    [ ( "x", VarBind (TyVar 0 1) ), ( "x", TyVarBind ) ]
                    (TyVar 1 2)
                    [ ( "x", VarBind (TyVar 0 1) ), ( "x", TyVarBind ) ]
                    (TyVar 0 1)
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



{-
   unifyTypeTest : Test
   unifyTypeTest =
       describe "unifyType"
           [ test "Should unify types" <|
               \_ ->
                   unifyType [] (TyName "TyVar1") [] (TyName "TyVar1")
                       |> Expect.equal (Ok [])
           , test "Should unify types 2" <|
               \_ ->
                   unifyType [] (TyAll "TyVar1" <| TyVar 0 1) [] (TyName "TyVar2")
                       |> Expect.equal (Ok [ ( TyName "TyVar2", -1 ) ])
           , test "Should unify types 3" <|
               \_ ->
                   unifyType [] (TyName "TyVar2") [] (TyAll "TyVar1" <| TyVar 0 1)
                       |> Expect.equal (Ok [ ( TyName "TyVar2", -1 ) ])
           , test "Should unify types 4" <|
               \_ ->
                   unifyType
                       []
                       (TyArr (TyName "TyVar2") (TyName "TyVar2"))
                       []
                       (TyAll "TyVar1" <| TyArr (TyVar 0 1) (TyVar 0 1))
                       |> Expect.equal (Ok [ ( TyName "TyVar2", "TyVar1" ) ])
           ]
-}
