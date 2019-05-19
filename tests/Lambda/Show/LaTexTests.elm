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
                    |> Expect.equal "\\AxiomC{$\\mathit{y}: \\textrm{Y}\\in \\{\\mathit{left}: \\textrm{A}, \\mathit{y}: \\textrm{Y}\\}$}\\RightLabel{\\textsc{(T–Var)}}\n\\UnaryInfC{$\\mathit{left}: \\textrm{A}, \\mathit{y}: \\textrm{Y}\\vdash \\mathit{y}: \\textrm{Y}$}\n\\AxiomC{$\\mathit{y}: \\textrm{Y}\\in \\{\\mathit{topRight}: \\textrm{A}, \\mathit{y}: \\textrm{Y}\\}$}\\RightLabel{\\textsc{(T–Var)}}\n\\UnaryInfC{$\\mathit{topRight}: \\textrm{A}, \\mathit{y}: \\textrm{Y}\\vdash \\mathit{y}: \\textrm{Y}$}\n\\AxiomC{$\\mathit{y}: \\textrm{Y}\\in \\{\\mathit{right}: \\textrm{A}, \\mathit{y}: \\textrm{Y}\\}$}\\RightLabel{\\textsc{(T–Var)}}\n\\BinaryInfC{$\\mathit{right}: \\textrm{A}, \\mathit{y}: \\textrm{Y}\\vdash \\mathit{y}: \\textrm{Y}$}\\RightLabel{\\textsc{(T–Abs)}}\n\\BinaryInfC{$\\mathit{bottom}: \\textrm{A}\\vdash \\lambda \\mathit{y}: \\textrm{Y}. \\mathit{y}: \\textrm{A}\\rightarrow \\textrm{B}$}"
        ]
