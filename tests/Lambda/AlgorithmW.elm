module Lambda.AlgorithmW exposing (..)

import Expect exposing (Expectation)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (..)
import Set
import Test exposing (..)


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
        , describe "TmAbs"
            [ test "lambda a. a" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        , ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        ]
                        (TmAbs I "termVar1" Nothing <| TmVar I 0 3)
                        |> Expect.equal (Ok <| ( [], TyArr (TyName "X") <| TyName "X" ))
            , test "lambda a. x" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        , ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        ]
                        (TmAbs I "termVar1" Nothing <| TmVar I 1 3)
                        |> Expect.equal (Ok <| ( [], TyArr (TyName "X") <| TyArr (TyName "A") <| TyName "A" ))
            ]
        , describe "TmApp"
            [ test "TmApp 1" <|
                \_ ->
                    w
                        [ ( "x", VarBind <| TyArr (TyName "A") <| TyName "A" )
                        , ( "id", VarBind <| (TyAll "A" <| TyArr (TyVar 0 1) <| TyVar 0 1) )
                        ]
                        (TmApp I (TmAbs I "termVar1" Nothing <| TmVar I 0 3) (TmVar I 0 2))
                        |> Result.map Tuple.second
                        -- What is the substitution good for here??
                        |> Expect.equal (Ok <| TyArr (TyName "A") <| TyName "A")
            , test "TmApp 2" <|
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
            [ test "TmLet 1" <|
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
        [ describe "TmLet"
            [ test "TmLet 1" <|
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
        ]
