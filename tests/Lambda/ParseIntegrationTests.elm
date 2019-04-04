module Lambda.ParseIntegrationTests exposing (..)

import Expect exposing (Expectation)
import Lambda.Expression as L
import Lambda.Parse as P
import Lambda.ParseTransform exposing (..)
import Result exposing (Result(..))
import Test exposing (..)


transformTextExprTest : Test
transformTextExprTest =
    describe "fromParseType"
        [ test "1" <|
            \_ ->
                P.parseCtx "TypeVar1, termVar1, TypeVar2, termVar2: TypeVar1"
                    |> Result.andThen
                        (\ctx ->
                            P.parseTerm "(lambda x: TypeVar1. x) termVar2"
                                |> Result.andThen
                                    (\term ->
                                        P.parseType "TypeVar1"
                                            |> Result.andThen
                                                (\ty -> Result.Ok ( ctx, term, ty ))
                                    )
                        )
                    |> Expect.equal
                        (Result.Ok <|
                            ( P.TyContext
                                [ P.TyVarBind "TypeVar1"
                                , P.VarBind "termVar1" Nothing
                                , P.TyVarBind "TypeVar2"
                                , P.VarBind "termVar2" (Just <| P.TyVar "TypeVar1")
                                ]
                            , P.TmApp
                                (P.TmAbs "x" (Just <| P.TyVar "TypeVar1") (P.TmVar "x"))
                                (P.TmVar "termVar2")
                            , P.TyVar "TypeVar1"
                            )
                        )
        , test "2" <|
            \_ ->
                P.parseCtx "TypeVar1, termVar1: TypeVar1, TypeVar2, termVar2: TypeVar1"
                    |> Result.andThen
                        (\ctx ->
                            P.parseTerm "(lambda x: TypeVar1. x) termVar2"
                                |> Result.andThen
                                    (\term ->
                                        P.parseType "TypeVar1"
                                            |> Result.andThen
                                                (\ty ->
                                                    Result.Ok
                                                        ( fromParseContext ctx
                                                        , fromParseContext ctx |> Result.andThen (\parsedCtx -> fromParseTerm parsedCtx term)
                                                        , fromParseContext ctx |> Result.andThen (\parsedCtx -> fromParseType parsedCtx ty)
                                                        )
                                                )
                                    )
                        )
                    |> Expect.equal
                        (Result.Ok <|
                            ( Ok [ ( "termVar2", L.VarBind (L.TyVar 2 3) ), ( "TypeVar2", L.TyVarBind ), ( "termVar1", L.VarBind (L.TyVar 0 1) ), ( "TypeVar1", L.TyVarBind ) ]
                            , Ok <|
                                L.TmApp L.I
                                    (L.TmAbs L.I "x" (Just <| L.TyVar 3 4) (L.TmVar L.I 0 5))
                                    (L.TmVar L.I 0 4)
                            , Ok <| L.TyVar 3 4
                            )
                        )
        ]
