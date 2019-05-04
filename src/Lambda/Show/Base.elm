module Lambda.Show.Base exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (addbinding, emptycontext, index2name)
import Lambda.Expression exposing (..)
import Lambda.Parse exposing (symbols)
import Lambda.Rule exposing (Rule(..))
import List.Extra


enclose : String -> String
enclose s =
    "(" ++ s ++ ")"


noEnclose : String -> String
noEnclose s =
    s


addBracketsTy : Ty -> String -> String
addBracketsTy ty =
    case ty of
        TyAll _ _ ->
            enclose

        TyArr _ _ ->
            enclose

        TyName _ ->
            noEnclose

        TyConst _ ->
            noEnclose

        TyVar _ _ ->
            noEnclose


addBracketsTermAppRight : Term -> String -> String
addBracketsTermAppRight t =
    case t of
        TmApp _ _ _ ->
            enclose

        TmIf _ _ _ _ ->
            enclose

        TmAbs _ _ _ _ ->
            enclose

        TmLet _ _ _ _ ->
            enclose

        TmTAbs _ _ _ ->
            enclose

        TmTApp _ _ _ ->
            enclose

        TmConst _ _ ->
            noEnclose

        TmVar _ _ _ ->
            noEnclose


addBracketsTermAppLeft : Term -> String -> String
addBracketsTermAppLeft t =
    case t of
        TmApp _ _ _ ->
            noEnclose

        TmIf _ _ _ _ ->
            enclose

        TmAbs _ _ _ _ ->
            enclose

        TmLet _ _ _ _ ->
            enclose

        TmTAbs _ _ _ ->
            enclose

        TmTApp _ _ _ ->
            noEnclose

        TmConst _ _ ->
            noEnclose

        TmVar _ _ _ ->
            noEnclose


addBracketsTermAppIf : Term -> String -> String
addBracketsTermAppIf t =
    case t of
        TmIf _ _ _ _ ->
            enclose

        TmLet _ _ _ _ ->
            enclose

        TmApp _ _ _ ->
            noEnclose

        TmAbs _ _ _ _ ->
            noEnclose

        TmTAbs _ _ _ ->
            noEnclose

        TmTApp _ _ _ ->
            noEnclose

        TmConst _ _ ->
            noEnclose

        TmVar _ _ _ ->
            noEnclose


addBracketsTermAppLetLeft : Term -> String -> String
addBracketsTermAppLetLeft t =
    case t of
        TmIf _ _ _ _ ->
            enclose

        TmLet _ _ _ _ ->
            enclose

        TmApp _ _ _ ->
            noEnclose

        TmAbs _ _ _ _ ->
            noEnclose

        TmTAbs _ _ _ ->
            noEnclose

        TmTApp _ _ _ ->
            noEnclose

        TmConst _ _ ->
            noEnclose

        TmVar _ _ _ ->
            noEnclose


addBracketsTermAppLetRight : Term -> String -> String
addBracketsTermAppLetRight t =
    case t of
        TmIf _ _ _ _ ->
            enclose

        TmLet _ _ _ _ ->
            noEnclose

        TmApp _ _ _ ->
            noEnclose

        TmAbs _ _ _ _ ->
            noEnclose

        TmTAbs _ _ _ ->
            noEnclose

        TmTApp _ _ _ ->
            noEnclose

        TmConst _ _ ->
            noEnclose

        TmVar _ _ _ ->
            noEnclose


addBracketsTermTypeApp : Term -> String -> String
addBracketsTermTypeApp t =
    case t of
        TmIf _ _ _ _ ->
            enclose

        TmLet _ _ _ _ ->
            enclose

        TmAbs _ _ _ _ ->
            enclose

        TmTAbs _ _ _ ->
            enclose

        TmApp _ _ _ ->
            noEnclose

        TmTApp _ _ _ ->
            noEnclose

        TmConst _ _ ->
            noEnclose

        TmVar _ _ _ ->
            noEnclose


showType : Context -> Ty -> String
showType ctx ty =
    case ty of
        TyConst TyBool ->
            "Bool"

        TyConst TyInt ->
            "Int"

        TyName varName ->
            varName

        TyAll varName ty1 ->
            symbols.forAll ++ varName ++ ". " ++ showType (addbinding ctx varName TyVarBind) ty1

        TyArr ty1 ty2 ->
            addBracketsTy ty1 (showType ctx ty1) ++ " " ++ symbols.arrow ++ " " ++ showType ctx ty2

        TyVar x _ ->
            index2name I ctx x
                |> Maybe.withDefault "IndexNotFoundInContext"


showTerm : Context -> Term -> String
showTerm ctx t =
    case t of
        TmVar fi x _ ->
            index2name fi ctx x
                |> Maybe.withDefault "indexNotFoundInContext"

        TmAbs _ varName maybeTy t1 ->
            let
                tyFormatted =
                    maybeTy
                        |> Maybe.map (showType ctx)
                        |> Maybe.map (\s -> ": " ++ s)
                        |> Maybe.withDefault ""
            in
            symbols.lambda
                ++ varName
                ++ tyFormatted
                ++ ". "
                ++ showTerm (addbinding ctx varName NameBind) t1

        TmApp _ t1 t2 ->
            addBracketsTermAppLeft t1 (showTerm ctx t1) ++ " " ++ addBracketsTermAppRight t2 (showTerm ctx t2)

        TmIf _ t1 t2 t3 ->
            "if "
                ++ addBracketsTermAppIf t1 (showTerm ctx t1)
                ++ " then "
                ++ addBracketsTermAppIf t2 (showTerm ctx t2)
                ++ " else "
                ++ addBracketsTermAppIf t3 (showTerm ctx t3)

        TmLet _ varName t1 t2 ->
            "let "
                ++ varName
                ++ " = "
                ++ addBracketsTermAppLetLeft t1 (showTerm ctx t1)
                ++ " in "
                ++ addBracketsTermAppLetRight t2 (showTerm (addbinding ctx varName NameBind) t2)

        TmTAbs _ varName t1 ->
            symbols.capitalLambda ++ varName ++ ". " ++ showTerm (addbinding ctx varName TyVarBind) t1

        TmTApp _ t1 ty ->
            addBracketsTermTypeApp t1 (showTerm ctx t1) ++ " [" ++ showType ctx ty ++ "]"

        TmConst _ TmTrue ->
            "true"

        TmConst _ TmFalse ->
            "false"


showCtx : Context -> String
showCtx =
    List.Extra.mapAccumr (\ctx ctxObj -> ( ctxObj :: ctx, showContextObject ctx ctxObj )) emptycontext
        >> Tuple.second
        >> List.reverse
        >> String.join ", "


showContextObject : Context -> ContextObject -> String
showContextObject ctx ( varName, binding ) =
    let
        showBinding b =
            case b of
                NameBind ->
                    ""

                TyVarBind ->
                    ""

                VarBind ty ->
                    ": " ++ showType ctx ty
    in
    varName ++ showBinding binding


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
