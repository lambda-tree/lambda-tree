module ParseTransformTests exposing (..)

import Either exposing (Either(..))
import Expect exposing (Expectation)
import Test exposing (..)
import ParseTransform exposing (..)
import Parse as P
import Lambda as L


fromParseContextTest : Test
fromParseContextTest =
    describe "fromParseContext"
        [ test "test empty context" <|
            \_ ->
                fromParseContext (P.TyContext [])
                    |> Expect.equal (Right [])
        , test "test non empty context" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" Nothing
                        ]
                    )
                    |> Expect.equal
                        (Right
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
                        (Right
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
                        (Right
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
                        (Right
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
                        (Right
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
                        (Left <| IndexNotFound "Nonexistent")
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
                    |> Expect.equal (Right <| L.TyVar 1 6)
        , test "should return error if variable not found in empty context" <|
            \_ ->
                fromParseType
                    []
                    (P.TyVar "TypeVar1")
                    |> Expect.equal (Left <| IndexNotFound "TypeVar1")
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
                    |> Expect.equal (Right <| L.TyArr (L.TyVar 1 6) (L.TyVar 5 6))
        , test "should return error for arrow type if variables are not found in context " <|
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
                    |> Expect.equal (Left <| IndexNotFound "TypeVar888")
        , test "should return error for arrow type if second variable is not found in context " <|
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
                    |> Expect.equal (Left <| IndexNotFound "TypeVar999")
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
                    |> Expect.equal (Right <| L.TyAll "TypeVar3" (L.TyVar 6 7))
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
                    |> Expect.equal (Right <| L.TyAll "TypeVar3" (L.TyVar 0 7))
        , test "should return error if variable is not found in context" <|
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
                    |> Expect.equal (Left <| IndexNotFound "TypeVar999")
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
                    |> Expect.equal (Right <| L.TmVar L.I 2 6)
        , test "TmVar should return error if variable not found in empty context" <|
            \_ ->
                fromParseTerm
                    []
                    (P.TmVar "termVar2")
                    |> Expect.equal (Left <| IndexNotFound "termVar2")
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
                    |> Expect.equal (Right <| L.TmAbs L.I "termVar5" (L.TyVar 5 6) (L.TmVar L.I 0 7))
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
                        (Right <|
                            L.TmApp L.I
                                (L.TmAbs L.I "termVar5" (L.TyVar 5 6) (L.TmVar L.I 0 7))
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
                        (Right <|
                            L.TmTAbs L.I "TypeVar5" <|
                                L.TmAbs L.I "termVar5" (L.TyVar 0 7) (L.TmVar L.I 0 8)
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
                        (Right <|
                            L.TmTApp L.I
                                (L.TmTAbs L.I
                                    "TypeVar5"
                                    (L.TmAbs L.I "termVar5" (L.TyVar 0 7) (L.TmVar L.I 0 8))
                                )
                                (L.TyArr (L.TyVar 1 6) (L.TyVar 5 6))
                        )
        ]
