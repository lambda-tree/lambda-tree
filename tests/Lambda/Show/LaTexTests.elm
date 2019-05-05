module Lambda.Show.LaTexTests exposing (..)

import Expect exposing (Expectation)
import Lambda.Expression exposing (..)
import Lambda.Rule exposing (Rule(..))
import Lambda.Show.LaTex exposing (showExprTree)
import Test exposing (..)
import Utils.Tree exposing (Tree(..))


showTest : Test
showTest =
    describe "show"
        [ test "should show simple expression" <|
            \_ ->
                showExprTree
                    (Node
                        { ctx = Ok <| [ ( "bottom", VarBind <| TyName "A" ) ]
                        , term = Ok <| TmAbs I "y" (Just <| TyName "Y") <| TmVar I 0 2
                        , ty = Ok <| TyArr (TyName "A") (TyName "B")
                        , rule = TAbs
                        }
                        [ Node
                            { ctx = Ok <| [ ( "y", VarBind <| TyName "Y" ), ( "left", VarBind <| TyName "A" ) ]
                            , term = Ok <| TmVar I 0 2
                            , ty = Ok <| TyName "Y"
                            , rule = TVar
                            }
                            []
                        , Node
                            { ctx = Ok <| [ ( "y", VarBind <| TyName "Y" ), ( "right", VarBind <| TyName "A" ) ]
                            , term = Ok <| TmVar I 0 2
                            , ty = Ok <| TyName "Y"
                            , rule = TVar
                            }
                            [ Node
                                { ctx = Ok <| [ ( "y", VarBind <| TyName "Y" ), ( "topRight", VarBind <| TyName "A" ) ]
                                , term = Ok <| TmVar I 0 2
                                , ty = Ok <| TyName "Y"
                                , rule = TVar
                                }
                                []
                            ]
                        ]
                    )
                    |> Expect.equal "\\mathit{x} : \\textrm{A} \\vdash \\mathit{x} : \\textrm{A} \\rightarrow \\textrm{B} "
        ]
