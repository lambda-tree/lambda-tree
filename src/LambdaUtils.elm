module LambdaUtils exposing (..)

import Lambda exposing (..)
import Context exposing (..)

{-| Shift indices by `d` if idx is greater than `c` in term `t`
-}
termShiftAbove : Int -> Int -> Term -> Term
termShiftAbove d ctx t =
    let
        shiftVar fi c x n =
            if x >= c then
                TmVar fi (x + d) (n + d)
            else
                TmVar fi x (n + d)
    in
        tmmap shiftVar (typeShiftAbove d) ctx t


{-| Shift indices by `d` in term `t`
-}
termShift : Int -> Term -> Term
termShift d t =
    termShiftAbove d 0 t


{-| Beta reduction of term or type variable.
NOTE: Not needed for working with types only.
-}
termSubst : Int -> Term -> Term -> Term
termSubst jIdx s t =
    let
        substVar fi j x n =
            if x == j then
                termShift j s
            else
                TmVar fi x n

        substType j tyT =
            tyT
    in
        tmmap substVar substType jIdx t


{-| Substitute var with idx `j` for `tyS` in term `t`.
-}
tytermSubst : Ty -> Int -> Term -> Term
tytermSubst tyS jIdx t =
    let
        substVar fi c x n =
            TmVar fi x n

        substType j tyT =
            typeSubst tyS j tyT
    in
        tmmap substVar substType jIdx t


{-| Beta reduction of term variable in a term.
-}
termSubstTop : Term -> Term -> Term
termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)


{-| Beta reduction of type variable in a term.
-}
tytermSubstTop : Ty -> Term -> Term
tytermSubstTop tyS t =
    termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)


{-| Map over term. Apply onvar and ontype functions with current ctx length.
-}
tmmap : (Info -> Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmmap onvar ontype ctx term =
    let
        walk c t =
            case t of
                TmVar fi x n ->
                    onvar fi c x n

                TmAbs fi x tyT1 t2 ->
                    TmAbs fi x (ontype c tyT1) (walk (c + 1) t2)

                TmApp fi t1 t2 ->
                    TmApp fi (walk c t1) (walk c t2)

                TmTAbs fi tyX t2 ->
                    TmTAbs fi tyX (walk (c + 1) t2)

                TmTApp fi t1 tyT2 ->
                    TmTApp fi (walk c t1) (ontype c tyT2)
    in
        walk ctx term



-- Chapter 25 - An ML Implementation of System F


{-| Shift indices by `d` if idx is greater than `c` in type `tyT`
-}
typeShiftAbove : Int -> Int -> Ty -> Ty
typeShiftAbove d c tyT =
    let
        shiftVar cc x n =
            if x >= cc then
                TyVar (x + d) (n + d)
            else
                TyVar x (n + d)
    in
        tymap shiftVar c tyT


{-| Shift all indices by `d` in type `tyT`
-}
typeShift : Int -> Ty -> Ty
typeShift d tyT =
    typeShiftAbove d 0 tyT


{-| Substitute type variable
with de Bruijn index `j`
for type `tyS`
in type `tyT`
-}
typeSubst : Ty -> Int -> Ty -> Ty
typeSubst tyS jIdx tyT =
    let
        substVar j x n =
            if x == j then
                -- The size of ctx is `j` => shift the substitution by `j`
                typeShift j tyS
            else
                TyVar x n
    in
        tymap substVar jIdx tyT


{-| Beta reduction step. Reduce with substitution for `tyS` in `tyT`
-}
typeSubstTop : Ty -> Ty -> Ty
typeSubstTop tyS tyT =
    -- Always substitute for the 0-th variable
    -- Shift the result so that the variable disappears
    typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)


{-| Map over type. Walk the type recursively and apply `onvar`(current ctx length, var idx, var ctx length) on variables
-}
tymap : (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tymap onvar ctx typeT =
    let
        walk c tyT =
            case tyT of
                TyVar x n ->
                    onvar c x n

                TyArr tyT1 tyT2 ->
                    TyArr (walk c tyT1) (walk c tyT2)

                TyAll tyX tyT2 ->
                    TyAll tyX (walk (c + 1) tyT2)

                TyName s ->
                    TyName s
    in
        walk ctx typeT



-- ---------------------------
