module Parse exposing (..)

import Parser exposing (..)
import Set
import Char


type Term
    = TmVar String
    | TmAbs String Ty Term
    | TmApp Term Term
    | TmTAbs String Term
    | TmTApp Term Ty


type Ty
    = TyVar String
    | TyArr Ty Ty
    | TyAll String Ty


keywords =
    [ "Lambda", "lambda", "let", "=", "in", "[", "]", "->", "forall", "Forall", ".", "(", ")", ":" ]


termVar : Parser String
termVar =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList keywords
        }


typeVar : Parser String
typeVar =
    variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList keywords
        }


termAbs : Parser Term
termAbs =
    succeed TmAbs
        |. keyword "lambda"
        |. spaces
        |= termVar
        |. spaces
        |. symbol ":"
        |. spaces
        |= typeExpr
        |. spaces
        |. symbol "."
        |. spaces
        |= lazy (\_ -> lambdaTermParser)
        |. spaces



-- Type expr


typeExpr : Parser Ty
typeExpr =
    typeSubExpr
        |> andThen (typeExprHelp [])


typeSubExpr : Parser Ty
typeSubExpr =
    oneOf
        [ succeed TyAll
            |. keyword "forall"
            |. spaces
            |= typeVar
            |. spaces
            |. symbol "."
            |. spaces
            |= lazy (\_ -> typeExpr)
        , succeed identity
            |= map TyVar typeVar
            |. spaces
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> typeExpr)
            |. spaces
            |. symbol ")"
            |. spaces
        ]


type TypeOperator
    = ArrOp



-- typeSubExpr consumes spaces at the end so that it doesn't need to start with `spaces` which cause problem in `oneOf`
-- if parser encounters '.' it should not consider it as an operator but it should end consuming
-- inspired by https://github.com/elm/parser/blob/master/examples/Math.elm


typeExprHelp : List ( Ty, TypeOperator ) -> Ty -> Parser Ty
typeExprHelp revOps expr =
    oneOf
        [ succeed Tuple.pair
            |= operator
            |. spaces
            |= typeSubExpr
            |> andThen (\( op, newExpr ) -> typeExprHelp (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]


operator : Parser TypeOperator
operator =
    oneOf
        [ map (\_ -> ArrOp) (symbol "->") ]


finalize : List ( Ty, TypeOperator ) -> Ty -> Ty
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, ArrOp ) :: otherRevOps ->
            finalize otherRevOps (TyArr expr finalExpr)



-- -------------------


lambdaTermParser : Parser Term
lambdaTermParser =
    succeed identity
        |. spaces
        |= (oneOf
                [ map TmVar termVar
                , termAbs
                ]
           )
        |. spaces
