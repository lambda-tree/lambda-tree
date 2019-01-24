module ParseTransform exposing (..)

import Parse as P
import Lambda exposing (..)
import Context exposing (..)
import ContextUtils exposing (..)
import Either exposing (Either(..))


type ParseError
    = IndexNotFound String
    | TypeMissing String
    | NotImplemented String


{-| Transforms Parsed context to lambda expression context
Note that Parse type context is ordered "left to right" but lambda Context is ordered "right to left"
-}
fromParseContext : P.TyContext -> Either ParseError Context
fromParseContext ctx =
    case ctx of
        P.TyContext bindings ->
            bindings
                |> List.foldl
                    (\binding ->
                        Either.andThen
                            (\someCtx ->
                                let
                                    transformed =
                                        case binding of
                                            P.VarBind name maybeType ->
                                                ( name
                                                , maybeType
                                                    |> Maybe.map (fromParseType someCtx)
                                                    |> Maybe.map (Either.mapRight VarBind)
                                                    |> Maybe.withDefault (Right NameBind)
                                                )

                                            P.TyVarBind name ->
                                                ( name, Right TyVarBind )

                                    eitherTransformed =
                                        case transformed of
                                            ( name, Right smt ) ->
                                                Right ( name, smt )

                                            ( name, Left err ) ->
                                                Left err
                                in
                                    eitherTransformed
                                        |> Either.mapRight (addbindingTup someCtx)
                            )
                    )
                    (Right emptycontext)


fromParseType : Context -> P.Ty -> Either ParseError Ty
fromParseType ctx ty =
    case ty of
        P.TyVar name ->
            case name2index I ctx name of
                Just index ->
                    Right <| TyVar index (ctxlength ctx)

                Nothing ->
                    Left (IndexNotFound name)

        P.TyArr ty1 ty2 ->
            fromParseType ctx ty1
                |> Either.andThenRight
                    (\t1 ->
                        fromParseType ctx ty2
                            |> Either.andThenRight
                                (\t2 -> Right <| TyArr t1 t2)
                    )

        P.TyAll name ty1 ->
            fromParseType (addbinding ctx name TyVarBind) ty1
                |> Either.mapRight (TyAll name)


fromParseTerm : Context -> P.Term -> Either ParseError Term
fromParseTerm ctx t =
    case t of
        P.TmVar name ->
            case name2index I ctx name of
                Just index ->
                    Right <| TmVar I index (ctxlength ctx)

                Nothing ->
                    Left (IndexNotFound name)

        P.TmAbs name maybeTy t1 ->
            maybeTy
                |> Maybe.map (fromParseType ctx)
                |> Maybe.withDefault (Left <| TypeMissing name)
                |> Either.andThenRight
                    (\ty ->
                        fromParseTerm (addbinding ctx name (VarBind ty)) t1
                            |> Either.andThenRight (\t1t -> Right <| TmAbs I name ty t1t)
                    )

        P.TmApp t1 t2 ->
            fromParseTerm ctx t1
                |> Either.andThenRight
                    (\t1t ->
                        fromParseTerm ctx t2
                            |> Either.andThenRight
                                (\t2t -> Right <| TmApp I t1t t2t)
                    )

        P.TmTAbs name t1 ->
            fromParseTerm (addbinding ctx name TyVarBind) t1
                |> Either.mapRight (TmTAbs I name)

        P.TmTApp t1 ty1 ->
            fromParseTerm ctx t1
                |> Either.andThenRight
                    (\t1t ->
                        fromParseType ctx ty1
                            |> Either.andThenRight
                                (\ty1t -> Right <| TmTApp I t1t ty1t)
                    )

        P.TmIf t1 t2 t3 ->
            Left <| NotImplemented "If-Then-Else"

        P.TmLet name t1 t2 ->
            Left <| NotImplemented "Let"
