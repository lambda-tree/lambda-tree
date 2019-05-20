module Lambda.Show.RuleTemplate exposing (..)

import Lambda.Rule exposing (Rule(..))
import Lambda.Show.Print exposing (Print(..))


type alias RuleTemplate =
    { tops : List Print, bottom : Print }


ruleTemplateForRule : Rule -> RuleTemplate
ruleTemplateForRule rule =
    case rule of
        TTrue ->
            { tops = [], bottom = Imply Gamma (OfType (Const "true") (Const "Bool")) }

        TFalse ->
            { tops = [], bottom = Imply Gamma (OfType (Const "false") (Const "Bool")) }

        TVar ->
            { tops =
                [ ElemOf (OfType (Var "x") Sigma) Gamma
                ]
            , bottom =
                Imply Gamma
                    (OfType (Var "x") Sigma)
            }

        TVarInst ->
            { tops =
                [ ElemOf (OfType (Var "x") Sigma) Gamma
                , Subset Sigma Tau
                ]
            , bottom =
                Imply Gamma
                    (OfType (Var "x") Tau)
            }

        TAbs ->
            { tops =
                [ Imply (AddToCtx Gamma (OfType (Var "x") Tau))
                    (OfType (SubExpr "M") (Tau |> Prime))
                ]
            , bottom =
                Imply Gamma
                    (OfType (Abs (Var "x") (SubExpr "M")) (Arrow Tau (Tau |> Prime)))
            }

        TApp ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") (Arrow Sigma Tau))
                , Imply Gamma
                    (OfType (SubExpr "N") Tau)
                ]
            , bottom =
                Imply Gamma
                    (OfType (Appl (SubExpr "M") (SubExpr "N")) Tau)
            }

        TIf ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") (Const "Bool"))
                , Imply Gamma
                    (OfType (SubExpr "N") Sigma)
                , Imply Gamma
                    (OfType (SubExpr "L") Sigma)
                ]
            , bottom =
                Imply Gamma
                    (OfType (IfThenElse (SubExpr "M") (SubExpr "N") (SubExpr "L")) Sigma)
            }

        TTAbs ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") Sigma)
                , NotElem (TypeVar "X") (FV Gamma)
                ]
            , bottom =
                Imply Gamma
                    (OfType (TypeAbs (TypeVar "X") (SubExpr "M")) (Forall (TypeVar "X") Sigma))
            }

        TTApp ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") (Forall (TypeVar "X") Sigma))
                ]
            , bottom =
                Imply Gamma
                    (OfType (TypeAppl (SubExpr "M") (Sigma |> Prime)) (SubstType (Sigma |> Prime) (TypeVar "X") Sigma))
            }

        TLet ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") Sigma)
                , Imply (AddToCtx Gamma (OfType (Var "x") Sigma))
                    (OfType (SubExpr "N") Tau)
                ]
            , bottom =
                Imply Gamma
                    (OfType (Let (Var "x") (SubExpr "M") (SubExpr "N")) Tau)
            }

        TLetGen ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") Sigma)
                , Imply (AddToCtx Gamma (OfType (Var "x") (Forall (ArrowOver Alpha) Tau)))
                    (OfType (SubExpr "N") (Tau |> Prime))
                ]
            , bottom =
                Imply Gamma
                    (OfType (Let (Var "x") (SubExpr "M") (SubExpr "N")) (Tau |> Prime))
            }

        TGen ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") Sigma)
                , NotElem (TypeVar "X") (FV Gamma)
                ]
            , bottom =
                Imply Gamma
                    (OfType (SubExpr "M") (Forall (TypeVar "X") Sigma))
            }

        TInst ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") (Sigma |> Prime))
                , Subset (Sigma |> Prime) Sigma
                ]
            , bottom =
                Imply Gamma
                    (OfType (SubExpr "M") Sigma)
            }

        NoRule ->
            { tops = [], bottom = Var "Not Applicable" }
