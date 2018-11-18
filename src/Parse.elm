module Parse exposing (..)

import Parser exposing (..)
import Set
import Char
import List.Extra


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


{-| Abstraction
-}
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
        |= lazy (\_ -> termExpr)


{-| Type abstraction
-}
typeAbs : Parser Term
typeAbs =
    succeed TmTAbs
        |. keyword "Lambda"
        |. spaces
        |= typeVar
        |. spaces
        |. symbol "."
        |. spaces
        |= lazy (\_ -> termExpr)


{-| Type expression
-}
typeExpr : Parser Ty
typeExpr =
    succeed identity
        |. spaces
        |= typeSubExpr
        |> andThen (\subExpr -> arrowExprHelp [ subExpr ])


{-| Type sub expression
TODO: Mark brackets with new value constructor?
-}
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



--


{-| Arrow expression helper

typeSubExpr consumes spaces at the end so that `arrowExprHelp` doesn't need to start with `spaces` that cause
problem in `oneOf`.
If parser encounters '.' it should not consider it as an operator but it should end consuming.
Inspired by <https://github.com/elm/parser/blob/master/examples/Math.elm>

`typeExprFinalize` is inside `lazy` so that it's not evaluated every time `arrowExprHelp` is called
(even when the 1st case of `oneOf` succeeds).

-}
arrowExprHelp : List Ty -> Parser Ty
arrowExprHelp revOps =
    oneOf
        [ succeed identity
            |. symbol "->"
            |. spaces
            |= typeSubExpr
            |> andThen (\op -> arrowExprHelp (op :: revOps))
        , lazy (\_ -> checkIfNoOperands <| arrowExprFinalize revOps)
        ]


{-| Applies operator on the reversed list of operands

Results in an expr. associated from right

-}
arrowExprFinalize : List Ty -> Maybe Ty
arrowExprFinalize revOps =
    List.Extra.foldl1 (\expr acc -> TyArr expr acc) revOps


checkIfNoOperands : Maybe a -> Parser a
checkIfNoOperands maybeResult =
    case maybeResult of
        Just result ->
            succeed result

        Nothing ->
            problem ("operator has no operands")



-- ----------------


{-| Term expression
-}
termExpr : Parser Term
termExpr =
    succeed identity
        |. spaces
        |= termSubExpr
        |> andThen appExprHelp


{-| Term sub expression
TODO: Mark brackets with new value constructor?
-}
termSubExpr : Parser Term
termSubExpr =
    oneOf
        [ map TmVar termVar
        , termAbs
        , typeAbs
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> termExpr)
            |. spaces
            |. symbol ")"
            |. spaces
        ]


{-| Type application sub expression
-}
termTyAppSubExpr : Parser Ty
termTyAppSubExpr =
    succeed identity
        |. symbol "["
        |. spaces
        |= typeExpr
        |. spaces
        |. symbol "]"


appExprHelp : Term -> Parser Term
appExprHelp term =
    succeed identity
        |. spaces
        |= oneOf
            [ oneOf
                [ map (TmApp term) termSubExpr
                , map (TmTApp term) termTyAppSubExpr
                ]
                |> andThen appExprHelp
            , lazy (\_ -> succeed term)
            ]
