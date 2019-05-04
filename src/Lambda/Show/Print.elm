module Lambda.Show.Print exposing (..)


type Print
    = Var String
    | Const String
    | TyVar String
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
    | Gamma
    | Sigma
    | Tau
    | Alpha
