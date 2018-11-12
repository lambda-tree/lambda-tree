module Parse exposing (..)

import Parser exposing (..)
import Set
import Char


type LTerm
    = LTmVar String
    | LTmAbs String LTy LTerm
    | LTmApp LTerm LTerm
    | LTmTAbs String LTerm
    | LTmTApp LTerm LTy


type LTy
    = LTyVar String
    | LTyArr LTy LTy
    | LTyAll String LTy


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


termAbs : Parser LTerm
termAbs =
    succeed LTmAbs
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


typeExpr : Parser LTy
typeExpr =
    typeSubExpr
        |> andThen (typeExprHelp [])


typeSubExpr : Parser LTy
typeSubExpr =
    oneOf
        [ succeed LTyAll
            |. keyword "forall"
            |. spaces
            |= typeVar
            |. spaces
            |. symbol "."
            |. spaces
            |= lazy (\_ -> typeExpr)
        , map LTyVar typeVar
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> typeExpr)
            |. spaces
            |. symbol ")"
        ]


type TypeOperator
    = ArrOp



-- try to remove the 2 backtrackables??
-- they're there to backtrack in e.g. lambda termVar1 : TypeVar1 . termVar1
-- if parser encounters '.' it should not consider it as an operator but it should end consuming
-- inspired by https://github.com/elm/parser/blob/master/examples/Math.elm


typeExprHelp : List ( LTy, TypeOperator ) -> LTy -> Parser LTy
typeExprHelp revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. backtrackable spaces
            |= backtrackable operator
            |. spaces
            |= typeSubExpr
            |> andThen (\( op, newExpr ) -> typeExprHelp (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]


operator : Parser TypeOperator
operator =
    oneOf
        [ map (\_ -> ArrOp) (symbol "->")
        ]


finalize : List ( LTy, TypeOperator ) -> LTy -> LTy
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, ArrOp ) :: otherRevOps ->
            finalize otherRevOps (LTyArr expr finalExpr)



-- -------------------


lambdaTermParser : Parser LTerm
lambdaTermParser =
    succeed identity
        |. spaces
        |= (oneOf
                [ map LTmVar termVar
                , termAbs
                ]
           )
        |. spaces
