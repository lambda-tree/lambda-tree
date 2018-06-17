module Lambda exposing (..)

{-| -}


{-| Type of term
`TyVar`(de Bruijn index, context length) - type variable use
`TyArr`(from type, to type) - function type
`TyAll`(variable name, in type) - universal quantification
-}
type Ty
    = TyVar Int Int
    | TyArr Ty Ty
    | TyAll String Ty


{-| Info dictionary
-}
type Info
    = I


{-| Term
-}
type Term
    = TmVar Info Int Int
    | TmAbs Info String Ty Term
    | TmApp Info Term Term
    | TmTAbs Info String Term
    | TmTApp Info Term Ty


{-| Binding.
NameBind - simple binding without type
VarBind(type of variable) - binding with type annotation
TyVarBind - type variable binding
-}
type Binding
    = NameBind
    | VarBind Ty
    | TyVarBind


{-| Shift indices by `d` if idx is greater than `c` in term `t`
-}
termShiftAbove d c t =
    let
        shiftVar fi c x n =
            if x >= c then
                TmVar fi (x + d) (n + d)
            else
                TmVar fi x (n + d)
    in
        tmmap shiftVar (typeShiftAbove d) c t


{-| Shift indices by `d` in term `t`
-}
termShift d t =
    termShiftAbove d 0 t


{-| Beta reduction of term or type variable.
NOTE: Not needed for working with types only.
-}
termSubst j s t =
    let
        substVar fi j x n =
            if x == j then
                termShift j s
            else
                TmVar fi x n

        substType j tyT =
            tyT
    in
        tmmap substVar substType j t


{-| Substitute var with idx `j` for `tyS` in term `t`.
-}
tytermSubst tyS j t =
    let
        substVar fi c x n =
            TmVar fi x n

        substType j tyT =
            typeSubst tyS j tyT
    in
        tmmap substVar substType j t


{-| Beta reduction of term variable in a term.
-}
termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)


{-| Beta reduction of type variable in a term.
-}
tytermSubstTop tyS t =
    termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)


{-| Map over term. Apply onvar and ontype functions with current ctx length.
-}
tmmap onvar ontype c t =
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
        walk c t



-- Chapter 25 - An ML Implementation of System F


{-| Shift indices by `d` if idx is greater than `c` in type `tyT`
-}
typeShiftAbove d c tyT =
    let
        shiftVar c x n =
            if x >= c then
                TyVar (x + d) (n + d)
            else
                TyVar x (n + d)
    in
        tymap shiftVar c tyT


{-| Shift all indices by `d` in type `tyT`
-}
typeShift d tyT =
    typeShiftAbove d 0 tyT


{-| Substitute type variable
with de Bruijn index `j`
for type `tyS`
in type `tyT`
-}
typeSubst tyS j tyT =
    let
        substVar j x n =
            if x == j then
                -- The size of ctx is `j` => shift the substitution by `j`
                typeShift j tyS
            else
                TyVar x n
    in
        tymap substVar j tyT


{-| Beta reduction step. Reduce with substitution for `tyS` in `tyT`
-}
typeSubstTop tyS tyT =
    -- Always substitute for the 0-th variable
    -- Shift the result so that the variable disappears
    typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)


{-| Map over type. Walk the type recursively and apply `onvar`(current ctx length, var idx, var ctx length) on variables
-}
tymap onvar c tyT =
    let
        walk c tyT =
            case tyT of
                TyVar x n ->
                    onvar c x n

                TyArr tyT1 tyT2 ->
                    TyArr (walk c tyT1) (walk c tyT2)

                TyAll tyX tyT2 ->
                    TyAll tyX (walk (c + 1) tyT2)
    in
        walk c tyT
