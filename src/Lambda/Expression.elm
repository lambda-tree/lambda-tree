module Lambda.Expression exposing (..)

{-| -}


{-| Type of term
`TyVar`(de Bruijn index - indexing from 0, context length) - type variable use
`TyArr`(from type, to type) - function type
`TyAll`(variable name, in type) - universal quantification
-}
type Ty
    = TyVar Int Int
    | TyArr Ty Ty
    | TyAll String Ty
    | TyName String


{-| Info dictionary
-}
type Info
    = I


type TmConst
    = TmTrue
    | TmFalse


{-| Term
-}
type Term
    = TmVar Info Int Int
    | TmAbs Info String Ty Term
    | TmApp Info Term Term
    | TmIf Info Term Term Term
    | TmTAbs Info String Term
    | TmTApp Info Term Ty
    | TmConst Info TmConst


{-| Binding.
NameBind - simple binding without type
VarBind(type of variable) - binding with type annotation
TyVarBind - type variable binding
-}
type Binding
    = NameBind
    | VarBind Ty
    | TyVarBind
