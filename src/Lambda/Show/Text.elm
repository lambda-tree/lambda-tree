module Lambda.Show.Text exposing (..)

import Lambda.Context exposing (Context)
import Lambda.Expression exposing (Term, Ty)
import Lambda.Show.Print exposing (..)


show : Print -> String
show print =
    case print of
        Var s ->
            s

        TypeVar s ->
            s

        Const s ->
            s

        TypeConst s ->
            s

        SubExpr s ->
            s

        OfType p1 p2 ->
            g [ show p1, ": ", show p2 ]

        ElemOf p1 p2 ->
            g [ show p1, " ∈ ", show p2 ]

        Imply p1 p2 ->
            g [ show p1, "⊢", show p2 ]

        Prime p1 ->
            g [ show p1, "'" ]

        Appl p1 p2 ->
            g [ show p1, " ", show p2 ]

        Subset p1 p2 ->
            g [ show p1, " ⊑ ", show p2 ]

        NotElem p1 p2 ->
            g
                [ show p1
                , " "
                , "\\∈"
                , " "
                , show p2
                ]

        FV p1 ->
            g [ "FV", "(", show p1, ")" ]

        Forall p1 p2 ->
            g [ "∀", show p1, ". ", show p2 ]

        Arrow p1 p2 ->
            g [ show p1, " → ", show p2 ]

        Abs p1 p2 ->
            g [ "λ", show p1, ". ", show p2 ]

        IfThenElse p1 p2 p3 ->
            g [ "if ", show p1, " then ", show p2, " else ", show p3 ]

        ArrowOver p1 ->
            g
                [ show p1
                , "^→"
                ]

        Let p1 p2 p3 ->
            g [ "let ", show p1, " = ", show p2, " in ", show p3 ]

        AddToCtx p1 p2 ->
            g [ show p1, ", ", show p2 ]

        SubstType p1 p2 p3 ->
            g [ "{", show p1, "/", show p2, "}", show p3 ]

        TypeAppl p1 p2 ->
            g [ show p1, " [", show p2, "]" ]

        TypeAbs p1 p2 ->
            g [ "Λ", show p1, ". ", show p2 ]

        Bracket p1 ->
            g [ "(", show p1, ")" ]

        Sequence p1 ->
            p1
                |> List.map show
                |> String.join ", "

        Gamma ->
            "Γ"

        Sigma ->
            "σ"

        Tau ->
            "τ"

        Alpha ->
            "α"


g =
    String.concat


showType : Context -> Ty -> String
showType ctx ty =
    Lambda.Show.Print.showType ctx ty
        |> show


showTerm : Context -> Term -> String
showTerm ctx ty =
    Lambda.Show.Print.showTerm ctx ty
        |> show
