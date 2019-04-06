module Lambda.ParseTransformTests exposing (..)

import Expect exposing (Expectation)
import Lambda.Expression as L
import Lambda.Parse as P
import Lambda.ParseTransform exposing (..)
import Result exposing (Result(..))
import Test exposing (..)


fromParseContextTest : Test
fromParseContextTest =
    describe "fromParseContext"
        [ test "test empty context" <|
            \_ ->
                fromParseContext (P.TyContext [])
                    |> Expect.equal (Ok [])
        , test "test non empty context" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" Nothing
                        ]
                    )
                    |> Expect.equal
                        (Ok
                            [ ( "x", L.NameBind )
                            , ( "X", L.TyVarBind )
                            ]
                        )
        , test "test non empty context with type variable reference" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" (Just <| P.TyVar "X")
                        ]
                    )
                    |> Expect.equal
                        (Ok
                            [ ( "x", L.VarBind (L.TyVar 0 1) )
                            , ( "X", L.TyVarBind )
                            ]
                        )
        , test "test non empty context with type" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" (Just <| P.TyAll "Z" (P.TyVar "Z"))
                        ]
                    )
                    |> Expect.equal
                        (Ok
                            [ ( "x", L.VarBind (L.TyAll "Z" (L.TyVar 0 2)) )
                            , ( "X", L.TyVarBind )
                            ]
                        )
        , test "test non empty context with type variable to context" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" (Just <| P.TyAll "Z" (P.TyVar "X"))
                        ]
                    )
                    |> Expect.equal
                        (Ok
                            [ ( "x", L.VarBind (L.TyAll "Z" (L.TyVar 1 2)) )
                            , ( "X", L.TyVarBind )
                            ]
                        )
        , test "test non empty context with multiple type vars" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" (Just <| P.TyAll "Z" <| P.TyAll "A" <| P.TyAll "B" <| P.TyVar "Z")
                        ]
                    )
                    |> Expect.equal
                        (Ok
                            [ ( "x", L.VarBind (L.TyAll "Z" <| L.TyAll "A" <| L.TyAll "B" <| L.TyVar 2 4) )
                            , ( "X", L.TyVarBind )
                            ]
                        )
        , test "test non empty context with non existent type" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" (Just <| P.TyAll "Z" (P.TyVar "Nonexistent"))
                        ]
                    )
                    |> Expect.equal
                        -- TODO: Should fail with error!
                        (Ok <| [ ( "x", L.VarBind (L.TyAll "Z" (L.TyVar 2 3)) ), ( "X", L.TyVarBind ) ])
        ]


fromParseTypeTest : Test
fromParseTypeTest =
    describe "fromParseType"
        [ test "should return variable if found in context" <|
            \_ ->
                fromParseType
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "termVar5", L.NameBind )
                    ]
                    (P.TyVar "TypeVar1")
                    |> .ty
                    |> Ok
                    |> Expect.equal (Ok <| L.TyVar 1 6)
        , test "should return free variable in context" <|
            \_ ->
                fromParseType
                    []
                    (P.TyVar "TypeVar1")
                    |> Expect.equal { ctx = [ ( "TypeVar1", L.TyVarBindFree ) ], ty = L.TyVar 0 1 }
        , test "should return arrow type if variables are found in context " <|
            \_ ->
                fromParseType
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "TypeVar2", L.TyVarBind )
                    ]
                    (P.TyArr (P.TyVar "TypeVar1") (P.TyVar "TypeVar2"))
                    |> .ty
                    |> Ok
                    |> Expect.equal (Ok <| L.TyArr (L.TyVar 1 6) (L.TyVar 5 6))
        , test "should return free variables in context if variables are not found in context " <|
            \_ ->
                fromParseType
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "TypeVar2", L.TyVarBind )
                    ]
                    (P.TyArr (P.TyVar "TypeVar888") (P.TyVar "TypeVar999"))
                    |> Expect.equal
                        { ctx =
                            [ ( "termVar1", L.NameBind )
                            , ( "TypeVar1", L.TyVarBind )
                            , ( "termVar2", L.NameBind )
                            , ( "termVar3", L.NameBind )
                            , ( "termVar4", L.NameBind )
                            , ( "TypeVar2", L.TyVarBind )
                            , ( "TypeVar888", L.TyVarBindFree )
                            , ( "TypeVar999", L.TyVarBindFree )
                            ]
                        , ty = L.TyArr (L.TyVar 6 7) (L.TyVar 7 8)
                        }
        , test "should return the free variable in context if second variable is not found in context " <|
            \_ ->
                fromParseType
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "TypeVar2", L.TyVarBind )
                    ]
                    (P.TyArr (P.TyVar "TypeVar1") (P.TyVar "TypeVar999"))
                    |> Expect.equal
                        { ctx =
                            [ ( "termVar1", L.NameBind )
                            , ( "TypeVar1", L.TyVarBind )
                            , ( "termVar2", L.NameBind )
                            , ( "termVar3", L.NameBind )
                            , ( "termVar4", L.NameBind )
                            , ( "TypeVar2", L.TyVarBind )
                            , ( "TypeVar999", L.TyVarBindFree )
                            ]
                        , ty = L.TyArr (L.TyVar 1 6) (L.TyVar 6 7)
                        }
        , test "should consider the expression's 'added' variable to the context in context length and deBruijn index" <|
            \_ ->
                fromParseType
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "TypeVar2", L.TyVarBind )
                    ]
                    (P.TyAll "TypeVar3" (P.TyVar "TypeVar2"))
                    |> .ty
                    |> Ok
                    |> Expect.equal (Ok <| L.TyAll "TypeVar3" (L.TyVar 6 7))
        , test "should return forall type correctly if the type's variable is referenced" <|
            \_ ->
                fromParseType
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "TypeVar2", L.TyVarBind )
                    ]
                    (P.TyAll "TypeVar3" (P.TyVar "TypeVar3"))
                    |> .ty
                    |> Ok
                    |> Expect.equal (Ok <| L.TyAll "TypeVar3" (L.TyVar 0 7))
        , test "should return variable in context if variable is not found in context" <|
            \_ ->
                fromParseType
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "TypeVar2", L.TyVarBind )
                    ]
                    (P.TyAll "TypeVar3" (P.TyVar "TypeVar999"))
                    |> Expect.equal
                        { ctx =
                            [ ( "termVar1", L.NameBind )
                            , ( "TypeVar1", L.TyVarBind )
                            , ( "termVar2", L.NameBind )
                            , ( "termVar3", L.NameBind )
                            , ( "termVar4", L.NameBind )
                            , ( "TypeVar2", L.TyVarBind )
                            , ( "TypeVar999", L.TyVarBindFree )
                            ]
                        , ty = L.TyAll "TypeVar3" (L.TyVar 7 8)
                        }
        ]


fromParseTermTest : Test
fromParseTermTest =
    describe "fromParseTerm"
        [ test "TmVar should return variable if found in context" <|
            \_ ->
                fromParseTerm
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "termVar5", L.NameBind )
                    ]
                    (P.TmVar "termVar2")
                    |> Expect.equal (Ok <| L.TmVar L.I 2 6)
        , test "TmVar should return error if variable not found in empty context" <|
            \_ ->
                fromParseTerm
                    []
                    (P.TmVar "termVar2")
                    |> Expect.equal (Err <| IndexNotFound "termVar2")
        , test "TmAbs should consider the expression's 'added' variable to the context in context length and deBruijn index" <|
            \_ ->
                fromParseTerm
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "TypeVar2", L.TyVarBind )
                    ]
                    (P.TmAbs "termVar5" (Just <| P.TyVar "TypeVar2") (P.TmVar "termVar5"))
                    |> Expect.equal (Ok <| L.TmAbs L.I "termVar5" (Just <| L.TyVar 5 6) (L.TmVar L.I 0 7))
        , test "TmApp should transform term application" <|
            \_ ->
                fromParseTerm
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "TypeVar2", L.TyVarBind )
                    ]
                    (P.TmApp
                        (P.TmAbs "termVar5" (Just <| P.TyVar "TypeVar2") (P.TmVar "termVar5"))
                        (P.TmVar "termVar2")
                    )
                    |> Expect.equal
                        (Ok <|
                            L.TmApp L.I
                                (L.TmAbs L.I "termVar5" (Just <| L.TyVar 5 6) (L.TmVar L.I 0 7))
                                (L.TmVar L.I 2 6)
                        )
        , test "TmTAbs should transform type abstraction" <|
            \_ ->
                fromParseTerm
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "TypeVar2", L.TyVarBind )
                    ]
                    (P.TmTAbs "TypeVar5" <| P.TmAbs "termVar5" (Just <| P.TyVar "TypeVar5") (P.TmVar "termVar5"))
                    |> Expect.equal
                        (Ok <|
                            L.TmTAbs L.I "TypeVar5" <|
                                L.TmAbs L.I "termVar5" (Just <| L.TyVar 0 7) (L.TmVar L.I 0 8)
                        )
        , test "TmTApp should transform type application" <|
            \_ ->
                fromParseTerm
                    [ ( "termVar1", L.NameBind )
                    , ( "TypeVar1", L.TyVarBind )
                    , ( "termVar2", L.NameBind )
                    , ( "termVar3", L.NameBind )
                    , ( "termVar4", L.NameBind )
                    , ( "TypeVar2", L.TyVarBind )
                    ]
                    (P.TmTApp
                        (P.TmTAbs "TypeVar5" <| P.TmAbs "termVar5" (Just <| P.TyVar "TypeVar5") (P.TmVar "termVar5"))
                        (P.TyArr (P.TyVar "TypeVar1") (P.TyVar "TypeVar2"))
                    )
                    |> Expect.equal
                        (Ok <|
                            L.TmTApp L.I
                                (L.TmTAbs L.I
                                    "TypeVar5"
                                    (L.TmAbs L.I "termVar5" (Just <| L.TyVar 0 7) (L.TmVar L.I 0 8))
                                )
                                (L.TyArr (L.TyVar 1 6) (L.TyVar 5 6))
                        )
        ]
