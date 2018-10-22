module LambdaUtilsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Lambda exposing (..)
import LambdaUtils exposing (..)


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
        TmAbs I "a" (TyVar 0 1) <|
            TmVar I 0 2


typeShiftAboveTest : Test
typeShiftAboveTest =
    describe "typeShiftAbove"
        [ test "Shifting Type" <|
            \_ ->
                typeShiftAbove (-1) 0 (TyAll "alpha" (TyArr (TyVar 1 1) (TyVar 1 1)))
                    |> Expect.equal (TyAll "alpha" (TyArr (TyVar 0 0) (TyVar 0 0)))
        ]

tytermSubstTopTest : Test
tytermSubstTopTest =
    describe "tytermSubstTop"
        [ test "T-Inst Step - System F" <|
            \_ ->
                tytermSubstTop (TyName "Bool") (TmAbs I "a" (TyVar 0 1) <| TmVar I 0 2)
                    |> Expect.equal (TmAbs I "a" (TyName "Bool") <| TmVar I 0 1)
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
                        (TyArr (TyName "Bool") (TyName "Bool"))
                        (TmVar I 0 2)
                    )
                    |> Expect.equal
                        (TmAbs I
                            "x"
                            (TyArr (TyName "Bool") (TyName "Bool"))
                            (TmVar I 0 4)
                        )
        , test "Should shift only free variable and not shift bound." <|
            \_ ->
                termShift 5
                    (TmApp I
                        (TmAbs I
                            "x"
                            (TyArr (TyName "Bool") (TyName "Bool"))
                            (TmVar I 0 2)
                        )
                        (TmVar I 1 2)
                    )
                    |> Expect.equal
                        (TmApp I
                            (TmAbs I
                                "x"
                                (TyArr (TyName "Bool") (TyName "Bool"))
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
                            (TyArr (TyName "Bool") (TyName "Bool"))
                            (TmVar I 0 2)
                        )
                        (TmVar I 1 2)
                    )
                    |> Expect.equal
                        (TmApp I
                            (TmAbs I
                                "x"
                                (TyArr (TyName "Bool") (TyName "Bool"))
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
                            (TyArr (TyName "Bool") (TyName "Bool"))
                            (TmVar I 0 2)
                        )
                        (TmVar I 1 2)
                    )
                    |> Expect.equal
                        (TmApp I
                            (TmAbs I
                                "x"
                                (TyArr (TyName "Bool") (TyName "Bool"))
                                (TmVar I 0 7)
                            )
                            (TmVar I 1 7)
                        )
        ]
