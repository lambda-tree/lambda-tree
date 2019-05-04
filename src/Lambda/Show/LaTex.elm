module Lambda.Show.LaTex exposing (..)

import Lambda.Rule exposing (Rule(..))
import Lambda.Show.Print exposing (..)


show : Print -> String
show print =
    case print of
        Var s ->
            italic s

        TypeVar s ->
            regular s

        Const s ->
            regular s

        TypeConst s ->
            regular s

        SubExpr s ->
            italic s

        OfType p1 p2 ->
            g [ show p1, ": ", show p2 ]

        ElemOf p1 p2 ->
            g [ show p1, const "in", show p2 ]

        Imply p1 p2 ->
            g [ show p1, const "vdash", show p2 ]

        Prime p1 ->
            g [ show p1, "'" ]

        Appl p1 p2 ->
            g [ show p1, "~", show p2 ]

        Subset p1 p2 ->
            g [ show p1, const "sqsubseteq", show p2 ]

        NotElem p1 p2 ->
            g
                [ show p1
                , consts [ "not", "in" ]
                , show p2
                ]

        FV p1 ->
            g [ regular "FV", "(", show p1, ")" ]

        Forall p1 p2 ->
            g [ const "forall", show p1, ". ", show p2 ]

        Arrow p1 p2 ->
            g [ show p1, const "rightarrow", show p2 ]

        Abs p1 p2 ->
            g [ const "lambda", show p1, ". ", show p2 ]

        IfThenElse p1 p2 p3 ->
            g [ regular "if ", show p1, regular " then ", show p2, regular " else ", show p3 ]

        ArrowOver p1 ->
            cmd "overrightarrow" [ show p1 ]

        Let p1 p2 p3 ->
            g [ regular "let ", show p1, " = ", show p2, regular " in ", show p3 ]

        AddToCtx p1 p2 ->
            g [ show p1, ", ", show p2 ]

        SubstType p1 p2 p3 ->
            g [ "{", show p1, "/", show p2, "}", show p3 ]

        TypeAppl p1 p2 ->
            g [ show p1, " [", show p2, "]" ]

        TypeAbs p1 p2 ->
            g [ const "Lambda", show p1, ". ", show p2 ]

        Bracket p1 ->
            g [ "(", show p1, ")" ]

        Sequence p1 ->
            p1
                |> List.map show
                |> String.join ", "

        Gamma ->
            const "Gamma"

        Sigma ->
            const "sigma"

        Tau ->
            const "tau"

        Alpha ->
            const "alpha"


g =
    String.concat


cmd : String -> List String -> String
cmd name args =
    "\\" ++ name ++ (args |> List.map (\x -> "{" ++ x ++ "}") |> String.concat) ++ " "


const : String -> String
const name =
    "\\" ++ name ++ " "


consts : List String -> String
consts names =
    names
        |> List.map (\x -> "\\" ++ x)
        |> String.concat
        |> (\x -> x ++ " ")


regular : String -> String
regular text =
    cmd "textrm" [ text ]


italic : String -> String
italic =
    identity


showRule : Rule -> String
showRule rule =
    case rule of
        TTrue ->
            "T–True"

        TFalse ->
            "T–False"

        TVar ->
            "T–Var"

        TVarInst ->
            "T–Var'"

        TAbs ->
            "T–Abs"

        TApp ->
            "T–App"

        TIf ->
            "T–If"

        TTAbs ->
            "T–TAbs"

        TTApp ->
            "T–TApp"

        TLet ->
            "T–Let"

        TLetGen ->
            "T–Let'"

        TGen ->
            "T–Gen"

        TInst ->
            "T–Inst"

        NoRule ->
            "-"
