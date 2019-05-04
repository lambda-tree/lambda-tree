module Lambda.Show.Print exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (addbinding, emptycontext, index2name)
import Lambda.Expression exposing (..)
import Lambda.Parse exposing (symbols)
import Lambda.Rule exposing (Rule(..))
import List.Extra


type Print
    = Var String
    | TypeVar String
    | Const String
    | TypeConst String
    | SubExpr String
    | OfType Print Print
    | ElemOf Print Print
    | Imply Print Print
    | Prime Print
    | Appl Print Print
    | Subset Print Print
    | NotElem Print Print
    | FV Print
    | Forall Print Print
    | Arrow Print Print
    | Abs Print Print
    | IfThenElse Print Print Print
    | ArrowOver Print
    | Let Print Print Print
    | AddToCtx Print Print
    | SubstType Print Print Print
    | TypeAppl Print Print
    | TypeAbs Print Print
    | Bracket Print
    | Sequence (List Print)
    | Gamma
    | Sigma
    | Tau
    | Alpha


enclose : Print -> Print
enclose =
    Bracket


noEnclose : Print -> Print
noEnclose =
    identity


addBracketsTy : Ty -> Print -> Print
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


addBracketsTermAppRight : Term -> Print -> Print
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


addBracketsTermAppLeft : Term -> Print -> Print
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


addBracketsTermAppIf : Term -> Print -> Print
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


addBracketsTermAppLetLeft : Term -> Print -> Print
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


addBracketsTermAppLetRight : Term -> Print -> Print
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


addBracketsTermTypeApp : Term -> Print -> Print
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


showType : Context -> Ty -> Print
showType ctx ty =
    case ty of
        TyConst TyBool ->
            TypeConst "Bool"

        TyConst TyInt ->
            TypeConst "Int"

        TyName varName ->
            TypeVar varName

        TyAll varName ty1 ->
            Forall (TypeVar varName) (showType (addbinding ctx varName TyVarBind) ty1)

        TyArr ty1 ty2 ->
            Arrow (addBracketsTy ty1 (showType ctx ty1)) (showType ctx ty2)

        TyVar x _ ->
            index2name I ctx x
                |> Maybe.withDefault "IndexNotFoundInContext"
                |> TypeVar


showTerm : Context -> Term -> Print
showTerm ctx t =
    case t of
        TmVar fi x _ ->
            index2name fi ctx x
                |> Maybe.withDefault "indexNotFoundInContext"
                |> Var

        TmAbs _ varName maybeTy t1 ->
            let
                maybeTyped =
                    maybeTy
                        |> Maybe.map (showType ctx)
                        |> Maybe.map (\x y -> OfType y x)
                        |> Maybe.withDefault identity
            in
            Abs (maybeTyped (Var varName)) (showTerm (addbinding ctx varName NameBind) t1)

        TmApp _ t1 t2 ->
            Appl (addBracketsTermAppLeft t1 (showTerm ctx t1)) (addBracketsTermAppRight t2 (showTerm ctx t2))

        TmIf _ t1 t2 t3 ->
            IfThenElse
                (addBracketsTermAppIf t1 (showTerm ctx t1))
                (addBracketsTermAppIf t2 (showTerm ctx t2))
                (addBracketsTermAppIf t3 (showTerm ctx t3))

        TmLet _ varName t1 t2 ->
            Let (Var varName)
                (addBracketsTermAppLetLeft t1 (showTerm ctx t1))
                (addBracketsTermAppLetRight t2 (showTerm (addbinding ctx varName NameBind) t2))

        TmTAbs _ varName t1 ->
            TypeAbs (TypeVar varName) (showTerm (addbinding ctx varName TyVarBind) t1)

        TmTApp _ t1 ty ->
            TypeAppl (addBracketsTermTypeApp t1 (showTerm ctx t1)) (showType ctx ty)

        TmConst _ TmTrue ->
            Const "true"

        TmConst _ TmFalse ->
            Const "false"


showCtx : Context -> Print
showCtx =
    List.Extra.mapAccumr (\ctx ctxObj -> ( ctxObj :: ctx, showContextObject ctx ctxObj )) emptycontext
        >> Tuple.second
        >> List.reverse
        >> Sequence


showContextObject : Context -> ContextObject -> Print
showContextObject ctx ( varName, binding ) =
    let
        showBinding b =
            case b of
                NameBind ->
                    identity

                TyVarBind ->
                    identity

                VarBind ty ->
                    \x -> OfType x (showType ctx ty)
    in
    showBinding binding (Var varName)
