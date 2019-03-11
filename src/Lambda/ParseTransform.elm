module Lambda.ParseTransform exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.Parse as P


type ParseTransformError
    = IndexNotFound String
    | TypeMissing String
    | NotImplemented String


{-| Transforms Parsed context to lambda expression context
Note that Parse type context is ordered "left to right" but lambda Context is ordered "right to left"
-}
fromParseContext : P.TyContext -> Result ParseTransformError Context
fromParseContext ctx =
    case ctx of
        P.TyContext bindings ->
            bindings
                |> List.foldl
                    (\binding ->
                        Result.andThen
                            (\someCtx ->
                                let
                                    transformed =
                                        case binding of
                                            P.VarBind name maybeType ->
                                                ( name
                                                , maybeType
                                                    |> Maybe.map (fromParseType someCtx)
                                                    |> Maybe.map (Result.map VarBind)
                                                    |> Maybe.withDefault (Ok NameBind)
                                                )

                                            P.TyVarBind name ->
                                                ( name, Ok TyVarBind )

                                    resultTransformed =
                                        case transformed of
                                            ( name, Ok smt ) ->
                                                Ok ( name, smt )

                                            ( name, Err err ) ->
                                                Err err
                                in
                                resultTransformed
                                    |> Result.map (addbindingTup someCtx)
                            )
                    )
                    (Ok emptycontext)


fromParseType : Context -> P.Ty -> Result ParseTransformError Ty
fromParseType ctx ty =
    case ty of
        P.TyVar name ->
            case name2index I ctx name of
                Just index ->
                    Ok <| TyVar index (ctxlength ctx)

                Nothing ->
                    case name of
                        "Bool" ->
                            Ok <| TyName "Bool"

                        "Int" ->
                            Ok <| TyName "Int"

                        _ ->
                            Err <| IndexNotFound name

        P.TyArr ty1 ty2 ->
            fromParseType ctx ty1
                |> Result.andThen
                    (\t1 ->
                        fromParseType ctx ty2
                            |> Result.andThen
                                (\t2 -> Ok <| TyArr t1 t2)
                    )

        P.TyAll name ty1 ->
            fromParseType (addbinding ctx name TyVarBind) ty1
                |> Result.map (TyAll name)


fromParseTerm : Context -> P.Term -> Result ParseTransformError Term
fromParseTerm ctx t =
    case t of
        P.TmVar name ->
            case name2index I ctx name of
                Just index ->
                    Ok <| TmVar I index (ctxlength ctx)

                Nothing ->
                    Err (IndexNotFound name)

        P.TmAbs name maybeTy t1 ->
            maybeTy
                |> Maybe.map (fromParseType ctx)
                |> Maybe.withDefault (Err <| TypeMissing name)
                |> Result.andThen
                    (\ty ->
                        fromParseTerm (addbinding ctx name (VarBind ty)) t1
                            |> Result.andThen (\t1t -> Ok <| TmAbs I name ty t1t)
                    )

        P.TmApp t1 t2 ->
            fromParseTerm ctx t1
                |> Result.andThen
                    (\t1t ->
                        fromParseTerm ctx t2
                            |> Result.andThen
                                (\t2t -> Ok <| TmApp I t1t t2t)
                    )

        P.TmTAbs name t1 ->
            fromParseTerm (addbinding ctx name TyVarBind) t1
                |> Result.map (TmTAbs I name)

        P.TmTApp t1 ty1 ->
            fromParseTerm ctx t1
                |> Result.andThen
                    (\t1t ->
                        fromParseType ctx ty1
                            |> Result.andThen
                                (\ty1t -> Ok <| TmTApp I t1t ty1t)
                    )

        P.TmIf t1 t2 t3 ->
            fromParseTerm ctx t1
                |> Result.andThen
                    (\t1t ->
                        fromParseTerm ctx t2
                            |> Result.andThen
                                (\t2t ->
                                    fromParseTerm ctx t3
                                        |> Result.andThen
                                            (\t3t -> Ok <| TmIf I t1t t2t t3t)
                                )
                    )

        P.TmLet name t1 t2 ->
            Err <| NotImplemented "Let"
