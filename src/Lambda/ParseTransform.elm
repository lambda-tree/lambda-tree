module Lambda.ParseTransform exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.Parse as P
import Lambda.TypeReturn exposing (TypeReturn, mapTy)
import Maybe.Extra


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
                                                    |> Maybe.map (.ty >> VarBind >> Ok)
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


fromParseType : Context -> P.Ty -> TypeReturn
fromParseType ctx ty =
    case ty of
        P.TyVar name ->
            case name2index I ctx name of
                Just index ->
                    { ctx = ctx, ty = TyVar index (ctxlength ctx) }

                Nothing ->
                    case name of
                        "Bool" ->
                            { ctx = ctx, ty = TyName "Bool" }

                        "Int" ->
                            { ctx = ctx, ty = TyName "Int" }

                        _ ->
                            { ctx = addFreeVar ctx name, ty = TyVar (ctxlength ctx) (ctxlength ctx + 1) }

        P.TyArr ty1 ty2 ->
            let
                r1 =
                    fromParseType ctx ty1

                r2 =
                    fromParseType r1.ctx ty2
            in
            { ctx = r2.ctx, ty = TyArr r1.ty r2.ty }

        P.TyAll name ty1 ->
            fromParseType (addbinding ctx name TyVarBind) ty1
                |> mapTy (TyAll name)


fromParseTerm : Context -> P.Term -> Result ParseTransformError Term
fromParseTerm ctx t =
    case t of
        P.TmVar name ->
            case name2index I ctx name of
                Just index ->
                    Ok <| TmVar I index (ctxlength ctx)

                Nothing ->
                    case name of
                        "true" ->
                            Ok <| TmConst I TmTrue

                        "false" ->
                            Ok <| TmConst I TmFalse

                        _ ->
                            Err (IndexNotFound name)

        P.TmAbs name maybeTy t1 ->
            let
                resultTyT =
                    case Maybe.map (Ok << .ty << fromParseType ctx) maybeTy of
                        Nothing ->
                            Ok Nothing

                        Just (Ok ty) ->
                            Ok <| Just ty

                        Just (Err err) ->
                            Err err

                binding =
                    resultTyT
                        |> Result.withDefault Nothing
                        |> Maybe.map VarBind
                        |> Maybe.withDefault NameBind
            in
            resultTyT
                |> Result.andThen
                    (\ty ->
                        fromParseTerm (addbinding ctx name binding) t1
                            |> Result.andThen
                                (\t1t ->
                                    Ok <| TmAbs I name ty t1t
                                )
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
                            |> .ty
                            |> (\ty1t -> Ok <| TmTApp I t1t ty1t)
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
            fromParseTerm ctx t1
                |> Result.andThen
                    (\t1t ->
                        fromParseTerm (addbinding ctx name NameBind) t2
                            |> Result.andThen
                                (\t2t -> Ok <| TmLet I name t1t t2t)
                    )
