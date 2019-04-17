module Lambda.ShowTests exposing (..)

import Expect exposing (Expectation)
import Lambda.Expression exposing (..)
import Lambda.Show exposing (..)
import Test exposing (..)


showTypeTest : Test
showTypeTest =
    describe "showType"
        [ test "should show Bool" <|
            \_ ->
                showType [] (TyConst TyBool)
                    |> Expect.equal "Bool"
        , test "should show Int" <|
            \_ ->
                showType [] (TyConst TyInt)
                    |> Expect.equal "Int"
        , test "should show free var" <|
            \_ ->
                showType [] (TyName "TyVarName1")
                    |> Expect.equal "TyVarName1"
        , test "should show bound var" <|
            \_ ->
                showType [ ( "TyVarName1", TyVarBind ) ] (TyVar 0 1)
                    |> Expect.equal "TyVarName1"
        , test "should show Forall A. Bool" <|
            \_ ->
                showType [] (TyAll "A" <| TyConst TyBool)
                    |> Expect.equal "∀A. Bool"
        , test "should show Forall A. Forall B. Bool" <|
            \_ ->
                showType [] (TyAll "A" <| TyAll "B" <| TyConst TyBool)
                    |> Expect.equal "∀A. ∀B. Bool"
        , test "should show A -> B" <|
            \_ ->
                showType [] (TyArr (TyName "A") <| TyName "B")
                    |> Expect.equal "A → B"
        , test "should show A -> B -> C" <|
            \_ ->
                showType [] (TyArr (TyName "A") <| TyArr (TyName "B") <| TyName "C")
                    |> Expect.equal "A → B → C"
        , test "should show (A -> B) -> C" <|
            \_ ->
                showType [] (TyArr (TyArr (TyName "A") <| TyName "B") <| TyName "C")
                    |> Expect.equal "(A → B) → C"
        , test "should show (∀A. ∀B. A -> B) -> C" <|
            \_ ->
                showType [] (TyArr (TyAll "A" <| TyAll "B" <| TyArr (TyName "A") <| TyName "B") <| TyName "C")
                    |> Expect.equal "(∀A. ∀B. A → B) → C"
        , test "should show (∀A. A) -> B -> C" <|
            \_ ->
                showType [] (TyArr (TyAll "A" <| TyName "A") <| TyArr (TyName "B") <| TyName "C")
                    |> Expect.equal "(∀A. A) → B → C"
        ]


showTermTest : Test
showTermTest =
    describe "showTerm"
        [ test "should show true" <|
            \_ ->
                showTerm [] (TmConst I TmTrue)
                    |> Expect.equal "true"
        , test "should show false" <|
            \_ ->
                showTerm [] (TmConst I TmFalse)
                    |> Expect.equal "false"
        , test "should show var" <|
            \_ ->
                showTerm [ ( "TyVarName1", TyVarBind ), ( "TyVarName2", TyVarBind ), ( "termVarName1", NameBind ), ( "termVarName2", NameBind ) ] (TmVar I 2 4)
                    |> Expect.equal "termVarName1"
        , test "should show if-then-else" <|
            \_ ->
                showTerm [] (TmIf I (TmConst I TmTrue) (TmConst I TmFalse) (TmConst I TmTrue))
                    |> Expect.equal "if true then false else true"
        , test "should show abstraction 1" <|
            \_ ->
                showTerm [] (TmAbs I "termVar1" Nothing (TmConst I TmTrue))
                    |> Expect.equal "λtermVar1. true"
        , test "should show abstraction 2" <|
            \_ ->
                showTerm [] (TmAbs I "termVar1" (Just <| TyConst TyBool) (TmConst I TmTrue))
                    |> Expect.equal "λtermVar1: Bool. true"
        , test "should show abstraction 3" <|
            \_ ->
                showTerm [] (TmAbs I "termVar1" (Just <| TyArr (TyConst TyBool) <| TyConst TyBool) (TmConst I TmTrue))
                    |> Expect.equal "λtermVar1: Bool → Bool. true"
        , test "should show application 1" <|
            \_ ->
                showTerm [ ( "termVar1", NameBind ) ] (TmApp I (TmVar I 0 1) <| TmConst I TmTrue)
                    |> Expect.equal "termVar1 true"
        , test "should show application 2" <|
            \_ ->
                showTerm [ ( "termVar1", NameBind ), ( "termVar2", NameBind ) ] (TmApp I (TmApp I (TmVar I 0 2) <| TmVar I 1 2) <| TmConst I TmTrue)
                    |> Expect.equal "termVar1 termVar2 true"
        , test "should show application 3" <|
            \_ ->
                showTerm [ ( "termVar1", NameBind ), ( "termVar2", NameBind ) ] (TmApp I (TmVar I 0 2) (TmApp I (TmVar I 1 2) <| TmConst I TmTrue))
                    |> Expect.equal "termVar1 (termVar2 true)"
        ]
