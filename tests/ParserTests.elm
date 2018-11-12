module ParserTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parser
import Result
import Parse exposing (..)


lambdaTermParserTest : Test
lambdaTermParserTest =
    describe "lambdaTermParser"
        [ test "should parse term variable correctly" <|
            \_ ->
                Parser.run lambdaTermParser "termVar1"
                    |> Expect.equal (Result.Ok <| TmVar "termVar1")
        ]


termVarTest : Test
termVarTest =
    describe "termVar"
        [ test "should parse term variable correctly" <|
            \_ ->
                Parser.run termVar "termVar1"
                    |> Expect.equal (Result.Ok <| "termVar1")
        ]



--typeVarTest : Test
--typeVarTest =
--    describe "typeVar"
--        [ test "should parse type variable correctly" <|
--            \_ ->
--                Parser.run termVar "TypeVar1"
--                    |> Expect.equal (Result.Ok <| "TypeVar1")
--        ]


typeExprTest : Test
typeExprTest =
    describe "typeExpr"
        [ test "should parse type variable" <|
            \_ ->
                Parser.run typeExpr "TypeVar1"
                    |> Expect.equal (Result.Ok <| TyVar "TypeVar1")
        , test "should parse type arrow" <|
            \_ ->
                Parser.run typeExpr "(TypeVar1 -> TypeVar2)"
                    |> Expect.equal (Result.Ok <| TyArr (TyVar "TypeVar1") (TyVar "TypeVar2"))
        , test "should parse type arrow without brackets" <|
            \_ ->
                Parser.run typeExpr "TypeVar1 -> TypeVar2"
                    |> Expect.equal (Result.Ok <| TyArr (TyVar "TypeVar1") (TyVar "TypeVar2"))
        , test "should parse multiple type arrows" <|
            \_ ->
                Parser.run typeExpr "TypeVar1 -> TypeVar2 -> TypeVar3"
                    |> Expect.equal (Result.Ok <| TyArr (TyVar "TypeVar1") (TyArr (TyVar "TypeVar2") (TyVar "TypeVar3")))
        , test "should parse type generalization" <|
            \_ ->
                Parser.run typeExpr "(forall TypeVar1. TypeVar1)"
                    |> Expect.equal (Result.Ok <| TyAll "TypeVar1" (TyVar "TypeVar1"))
        , test "should parse type generalization without brackets" <|
            \_ ->
                Parser.run typeExpr "forall TypeVar1. TypeVar1"
                    |> Expect.equal (Result.Ok <| TyAll "TypeVar1" (TyVar "TypeVar1"))

        --        , test "experiment" <|
        --            \_ ->
        --                Parser.run typeExpr "forall TypeVar1. TypeVar1 -> TypeVar1"
        --                    |> Expect.equal (Result.Ok <| TyAll "TypeVar1" (TyVar "TypeVar1"))
        , test "should parse complex expr." <|
            \_ ->
                Parser.run typeExpr "forall TypeVar1. forall TypeVar2. TypeVar1 -> (TypeVar1 -> TypeVar2) -> TypeVar2"
                    |> Expect.equal
                        (Result.Ok <|
                            TyAll "TypeVar1"
                                (TyAll "TypeVar2"
                                    (TyArr
                                        (TyVar "TypeVar1")
                                        (TyArr
                                            (TyArr
                                                (TyVar "TypeVar1")
                                                (TyVar "TypeVar2")
                                            )
                                            (TyVar "TypeVar2")
                                        )
                                    )
                                )
                        )
        ]


termAbsTest : Test
termAbsTest =
    describe "termAbs"
        [ test "should parse term abstraction" <|
            \_ ->
                Parser.run termAbs "lambda termVar1 : TypeVar1 . termVar1"
                    |> Expect.equal (Result.Ok <| TmAbs "termVar1" (TyVar "TypeVar1") (TmVar "termVar1"))
        ]
