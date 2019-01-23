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
                    |> Expect.equal []
        , test "test non empty context" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" Nothing
                        ]
                    )
                    |> Expect.equal
                        [ ( "x", L.NameBind )
                        , ( "X", L.TyVarBind )
                        ]
        , test "test non empty context with type variable reference" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" (Just <| P.TyVar "X")
                        ]
                    )
                    |> Expect.equal
                        [ ( "x", L.VarBind (L.TyVar 0 1) )
                        , ( "X", L.TyVarBind )
                        ]
        , test "test non empty context with type" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" (Just <| P.TyAll "Z" (P.TyVar "Z"))
                        ]
                    )
                    |> Expect.equal
                        [ ( "x", L.VarBind (L.TyAll "Z" (L.TyVar 0 2)) )
                        , ( "X", L.TyVarBind )
                        ]
        , test "test non empty context with type variable to context" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" (Just <| P.TyAll "Z" (P.TyVar "X"))
                        ]
                    )
                    |> Expect.equal
                        [ ( "x", L.VarBind (L.TyAll "Z" (L.TyVar 1 2)) )
                        , ( "X", L.TyVarBind )
                        ]
        , test "test non empty context with multiple type vars" <|
            \_ ->
                fromParseContext
                    (P.TyContext
                        [ P.TyVarBind "X"
                        , P.VarBind "x" (Just <| P.TyAll "Z" <| P.TyAll "A" <| P.TyAll "B" <| P.TyVar "Z")
                        ]
                    )
                    |> Expect.equal
                        [ ( "x", L.VarBind (L.TyAll "Z" <| L.TyAll "A" <| L.TyAll "B" <| L.TyVar 2 4) )
                        , ( "X", L.TyVarBind )
                        ]
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
                        [ ( "x", L.NameBind )
                        , ( "X", L.TyVarBind )
                        ]
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
