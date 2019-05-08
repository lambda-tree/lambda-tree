module Lambda.InfererTests exposing (..)

import Expect exposing (Expectation)
import Lambda.Expression exposing (..)
import Lambda.Inferer exposing (inferTree, typeOf, w)
import Lambda.Rule exposing (Rule(..))
import Set
import Test exposing (..)
import Utils.Outcome exposing (Outcome(..))
import Utils.Tree exposing (Tree(..))


inferTreeTest : Test
inferTreeTest =
    describe "inferTree"
        [ test "should build tree" <|
            \_ ->
                inferTree (HM SyntaxDirected) Set.empty Nothing [] (TmConst I TmTrue)
                    |> Expect.equal
                        (Outcome Nothing <|
                            Node
                                { ctx = []
                                , term = TmConst I TmTrue
                                , ty = TyConst TyBool
                                , rule = TTrue
                                , ss = []
                                , ftvs = Set.empty
                                }
                                []
                        )
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
                        |> Expect.equal (Ok <| ( [], TyArr (TyName "A1") <| TyName "A1" ))
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
                        |> Expect.equal (Ok <| TyArr (TyName "X2") <| TyArr (TyName "X11") <| TyName "X2")
            ]
        ]


typeOfTest : Test
typeOfTest =
    describe "typeOf"
        [ describe "H-M"
            [ test "Let const = lambda x. lambda y. x in const : Forall X, X1. X -> X1 -> X" <|
                \_ ->
                    typeOf (HM SyntaxDirected)
                        []
                        (TmLet I
                            "const"
                            (TmAbs I "termVar1" Nothing <| TmAbs I "termVar2" Nothing <| TmVar I 1 2)
                            (TmVar I 0 1)
                        )
                        |> Expect.equal (Ok <| TyAll "X11" <| TyAll "X2" <| TyArr (TyVar 0 2) <| TyArr (TyVar 1 2) <| TyVar 0 2)
            ]
        , describe "System F"
            [ test "Lambda A . lambda f: A -> A . lambda x: A. f (f x): Forall A. (A -> A) -> A -> A" <|
                \_ ->
                    typeOf SystemF
                        []
                        (TmTAbs I "A" <|
                            TmAbs I "f" (Just <| TyArr (TyVar 0 1) <| TyVar 0 1) <|
                                TmAbs I "x" (Just <| TyVar 1 2) <|
                                    (TmApp I (TmVar I 1 3) <| TmApp I (TmVar I 1 3) <| TmVar I 0 3)
                        )
                        |> Expect.equal (Ok <| TyAll "A" <| TyArr (TyArr (TyVar 0 1) <| TyVar 0 1) <| TyArr (TyVar 0 1) <| TyVar 0 1)
            , test "Lambda A. lambda termVar1: A. lambda termVar2: A. termVar1: Forall A. A -> A -> A" <|
                \_ ->
                    typeOf SimplyTyped
                        []
                        (TmTAbs I "A" <|
                            (TmAbs I "termVar1" (Just <| TyVar 0 1) <| TmAbs I "termVar2" (Just <| TyVar 1 2) <| TmVar I 1 3)
                        )
                        |> Expect.equal (Ok <| TyAll "A" <| TyArr (TyVar 0 1) <| TyArr (TyVar 0 1) <| TyVar 0 1)
            , test "Lambda A. Lambda B. lambda termVar1: A. lambda termVar2: B. termVar1: Forall A. Forall B. A -> B -> A" <|
                \_ ->
                    typeOf SystemF
                        []
                        (TmTAbs I "A" <|
                            TmTAbs I "B" <|
                                (TmAbs I "termVar1" (Just <| TyVar 1 2) <| TmAbs I "termVar2" (Just <| TyVar 1 3) <| TmVar I 1 4)
                        )
                        |> Expect.equal (Ok <| TyAll "A" <| TyAll "B" <| TyArr (TyVar 1 2) <| TyArr (TyVar 0 2) <| TyVar 1 2)
            , test "(Lambda A. Lambda B. lambda termVar1: A. lambda termVar2: B. termVar1) [Bool]: Forall B. Bool -> B -> Bool" <|
                \_ ->
                    typeOf SystemF
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
