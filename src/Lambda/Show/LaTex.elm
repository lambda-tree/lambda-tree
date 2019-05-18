module Lambda.Show.LaTex exposing (..)

import Lambda.Rule exposing (ExprTree, ExprTreeContent, Rule(..), TypeStatement)
import Lambda.Show.Print exposing (..)
import Utils.Tree exposing (Tree(..))


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

        SequenceSet p1 ->
            g [ "\\{", show p1, "\\}" ]

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
cmd name contents =
    cmdWithArgs name [] contents


cmdWithArgs : String -> List String -> List String -> String
cmdWithArgs name args contents =
    "\\"
        ++ name
        ++ (case args of
                [] ->
                    ""

                _ ->
                    "[" ++ String.join ", " args ++ "]"
           )
        ++ (contents
                |> List.map (\x -> "{" ++ x ++ "}")
                |> String.concat
           )


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
italic text =
    cmd "mathit" [ text ]


math : String -> String
math text =
    "$" ++ text ++ "$"


error : String -> String
error text =
    cmd "textcolor" [ "red", cmd "textbf" [ text ] ]


showRule : Rule -> String
showRule rule =
    let
        text =
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
    in
    cmd "textsc" [ g [ "(", text, ")" ] ]


type ShowTreeContent
    = Judgement ExprTreeContent
    | Element TypeStatement


showTreeContent : ShowTreeContent -> String
showTreeContent c =
    case c of
        Judgement content ->
            case ( content.ctx, content.term, content.ty ) of
                ( Ok ctx, Ok term, Ok ty ) ->
                    Imply (showCtx ctx) (OfType (showTerm ctx term) (showType ctx ty))
                        |> show

                _ ->
                    error "Error parsing context, term or type"

        Element { ctx, term, ty } ->
            ElemOf
                (OfType (showTerm ctx term)
                    (showType ctx ty)
                )
                (SequenceSet <| showCtx ctx)
                |> show


isTerminalRule : Rule -> Bool
isTerminalRule rule =
    case rule of
        TTrue ->
            True

        TFalse ->
            True

        TVar ->
            False

        TVarInst ->
            True

        TAbs ->
            False

        TApp ->
            False

        TIf ->
            False

        TTAbs ->
            False

        TTApp ->
            False

        TLet ->
            False

        TLetGen ->
            False

        TGen ->
            False

        TInst ->
            False

        NoRule ->
            False


showExprTree : ExprTree -> String
showExprTree tree =
    tree
        |> Utils.Tree.mapTree
            (\(Node content c) ->
                case content.rule of
                    TVar ->
                        case ( content.ctx, content.term, content.ty ) of
                            ( Ok ctx, Ok term, Ok ty ) ->
                                Node (Judgement content)
                                    [ Node (Element { ctx = ctx, term = term, ty = ty }) [] ]

                            _ ->
                                Node (Judgement content) []

                    _ ->
                        Node (Judgement content) []
            )
        |> Utils.Tree.foldTree
            (\content subPrints ->
                showTreeContent content
                    |> (\x ->
                            case content of
                                Judgement c ->
                                    if isTerminalRule c.rule then
                                        x ++ (regular <| "~" ++ showRule c.rule)

                                    else
                                        x

                                _ ->
                                    x
                       )
                    |> cmdForSubTreeCount (List.length subPrints)
                    |> (\x ->
                            case content of
                                Judgement c ->
                                    if isTerminalRule c.rule then
                                        x

                                    else
                                        cmd "RightLabel" [ showRule c.rule ]
                                            ++ "\n"
                                            ++ x

                                _ ->
                                    x
                       )
                    |> (\x -> String.join "\n" subPrints ++ x)
            )


cmdForSubTreeCount : Int -> String -> String
cmdForSubTreeCount subTreeCount text =
    case subTreeCount of
        0 ->
            cmd "AxiomC" [ math text ]

        1 ->
            cmd "UnaryInfC" [ math text ]

        2 ->
            cmd "BinaryInfC" [ math text ]

        3 ->
            cmd "TrinaryInfC" [ math text ]

        4 ->
            cmd "QuaternaryInfC" [ math text ]

        5 ->
            cmd "QuinaryInfC" [ math text ]

        _ ->
            error "Too many proof children. Max supported count is 5"


wrapProofTreeForExport : String -> String
wrapProofTreeForExport proofTree =
    [ cmdWithArgs "documentclass" [ "border=20pt" ] [ "standalone" ]
    , cmdWithArgs "usepackage" [ "utf8" ] [ "inputenc" ]
    , cmd "usepackage" [ "amsmath" ]
    , cmd "usepackage" [ "bussproofs" ]
    , cmd "usepackage" [ "xcolor" ]
    , ""
    , cmd "begin" [ "document" ]
    , ""
    , proofTree
    , ""
    , cmd "DisplayProof" []
    , ""
    , cmd "end" [ "document" ]
    ]
        |> String.join "\n"
