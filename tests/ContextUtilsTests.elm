module ContextUtilsTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import ParseTransform exposing (..)
import Context exposing (..)
import ContextUtils exposing (..)
import Lambda exposing (..)


ctxlengthTest : Test
ctxlengthTest =
    describe "ctxlength"
        [ test "ctxlength of empty context should be 0" <|
            \_ ->
                ctxlength emptycontext
                    |> Expect.equal 0
        , test "ctxlength of context with 1 binding should be 1" <|
            \_ ->
                ctxlength [ ( "termVar1", NameBind ) ]
                    |> Expect.equal 1
        , test "ctxlength of context with 3 bindings should be 3" <|
            \_ ->
                ctxlength [ ( "termVar2", VarBind <| TyVar 0 2 ), ( "TypeVar1", TyVarBind ), ( "termVar1", NameBind ) ]
                    |> Expect.equal 3
        ]


addbindingTest : Test
addbindingTest =
    describe "addbinding"
        [ test "addbinding to empty context should contain just it" <|
            \_ ->
                addbinding [] "TypeVar1" TyVarBind
                    |> Expect.equal [ ( "TypeVar1", TyVarBind ) ]
        , test "addbinding to non-empty context should put it to head of the ctx" <|
            \_ ->
                addbinding [ ( "termVar1", NameBind ) ] "TypeVar1" TyVarBind
                    |> Expect.equal [ ( "TypeVar1", TyVarBind ), ( "termVar1", NameBind ) ]
        ]


isnameboundTest : Test
isnameboundTest =
    describe "isnamebound"
        [ test "should return True if name is bound" <|
            \_ ->
                isnamebound [ ( "TypeVar1", TyVarBind ), ( "termVar1", NameBind ) ] "termVar1"
                    |> Expect.equal True
        , test "should return False if not bound" <|
            \_ ->
                isnamebound [ ( "TypeVar1", TyVarBind ), ( "termVar1", NameBind ) ] "termVar2"
                    |> Expect.equal False
        , test "should return False if context empty" <|
            \_ ->
                isnamebound [] "termVar1"
                    |> Expect.equal False
        ]


pickfreshnameTest : Test
pickfreshnameTest =
    describe "pickfreshname"
        [ test "should return same the name if fresh" <|
            \_ ->
                pickfreshname
                    [ ( "TypeVar1", TyVarBind )
                    , ( "termVar1", NameBind )
                    ]
                    "termVar2"
                    |> Expect.equal
                        ( [ ( "termVar2", NameBind )
                          , ( "TypeVar1", TyVarBind )
                          , ( "termVar1", NameBind )
                          ]
                        , "termVar2"
                        )
        , test "should return the name with apostrophe if not fresh" <|
            \_ ->
                pickfreshname
                    [ ( "TypeVar1", TyVarBind )
                    , ( "termVar1", NameBind )
                    ]
                    "termVar1"
                    |> Expect.equal
                        ( [ ( "termVar1'", NameBind )
                          , ( "TypeVar1", TyVarBind )
                          , ( "termVar1", NameBind )
                          ]
                        , "termVar1'"
                        )
        ]


index2nameTest : Test
index2nameTest =
    describe "index2name"
        [ test "should return the head name if index is 0" <|
            \_ ->
                index2name I
                    [ ( "termVar1'", NameBind )
                    , ( "TypeVar1", TyVarBind )
                    , ( "termVar1", NameBind )
                    ]
                    0
                    |> Expect.equal (Just "termVar1'")
        , test "should return element after head from start if index is 1" <|
            \_ ->
                index2name I
                    [ ( "termVar1'", NameBind )
                    , ( "TypeVar1", TyVarBind )
                    , ( "termVar1", NameBind )
                    , ( "termVar0", NameBind )
                    , ( "termVar3", NameBind )
                    , ( "termVar4", NameBind )
                    ]
                    1
                    |> Expect.equal (Just "TypeVar1")
        , test "should return nothing if index is >= length" <|
            \_ ->
                index2name I
                    [ ( "termVar1'", NameBind )
                    , ( "TypeVar1", TyVarBind )
                    , ( "termVar1", NameBind )
                    , ( "termVar0", NameBind )
                    , ( "termVar3", NameBind )
                    , ( "termVar4", NameBind )
                    ]
                    6
                    |> Expect.equal (Nothing)
        ]


getbindingTest : Test
getbindingTest =
    describe "getbinding"
        [ test "should return the head binding if index is 0" <|
            \_ ->
                getbinding
                    [ ( "termVar1'", NameBind )
                    , ( "TypeVar1", TyVarBind )
                    , ( "termVar1", NameBind )
                    ]
                    0
                    |> Expect.equal (Just NameBind)
        , test "should return element after head from start if index is 1" <|
            \_ ->
                getbinding
                    [ ( "termVar1'", NameBind )
                    , ( "TypeVar1", TyVarBind )
                    , ( "termVar1", NameBind )
                    , ( "termVar0", NameBind )
                    , ( "termVar3", NameBind )
                    , ( "termVar4", NameBind )
                    ]
                    1
                    |> Expect.equal (Just TyVarBind)
        , test "should return nothing if index is >= length" <|
            \_ ->
                getbinding
                    [ ( "termVar1'", NameBind )
                    , ( "TypeVar1", TyVarBind )
                    , ( "termVar1", NameBind )
                    , ( "termVar0", NameBind )
                    , ( "termVar3", NameBind )
                    , ( "termVar4", NameBind )
                    ]
                    6
                    |> Expect.equal (Nothing)
        ]


name2indexTest : Test
name2indexTest =
    describe "name2index"
        [ test "should return 0 if binding is the head" <|
            \_ ->
                name2index I
                    [ ( "termVar1", NameBind )
                    , ( "TypeVar1", TyVarBind )
                    , ( "termVar2", NameBind )
                    ]
                    "termVar1"
                    |> Expect.equal (Just 0)
        , test "should return index from start if index is higher" <|
            \_ ->
                name2index I
                    [ ( "termVar1", NameBind )
                    , ( "TypeVar1", TyVarBind )
                    , ( "termVar2", NameBind )
                    , ( "termVar3", NameBind )
                    , ( "termVar4", NameBind )
                    , ( "termVar5", NameBind )
                    ]
                    "termVar4"
                    |> Expect.equal (Just 4)
        , test "should return nothing if name is not in context" <|
            \_ ->
                name2index I
                    [ ( "termVar1'", NameBind )
                    , ( "TypeVar1", TyVarBind )
                    , ( "termVar1", NameBind )
                    , ( "termVar0", NameBind )
                    , ( "termVar3", NameBind )
                    , ( "termVar4", NameBind )
                    ]
                    "termVar393"
                    |> Expect.equal (Nothing)
        ]
