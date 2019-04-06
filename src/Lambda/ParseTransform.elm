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
                                                    |> Maybe.map VarBind
                                                    |> Maybe.withDefault NameBind
                                                )

                                            P.TyVarBind name ->
                                                ( name, TyVarBind )
                                in
                                Ok <| addbindingTup someCtx transformed
                            )
                    )
                    (Ok emptycontext)


fromParseType : Context -> P.Ty -> Ty
fromParseType ctx ty =
    case ty of
        P.TyVar name ->
            case name2index I ctx name of
                Just index ->
                    TyVar index (ctxlength ctx)

                Nothing ->
                    case name of
                        "Bool" ->
                            TyName "Bool"

                        "Int" ->
                            TyName "Int"

                        _ ->
                            TyName name

        P.TyArr ty1 ty2 ->
            TyArr (fromParseType ctx ty1) (fromParseType ctx ty2)

        P.TyAll name ty1 ->
            TyAll name (fromParseType (addbinding ctx name TyVarBind) ty1)


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
                    Maybe.map (fromParseType ctx) maybeTy

                binding =
                    resultTyT
                        |> Maybe.map VarBind
                        |> Maybe.withDefault NameBind
            in
            fromParseTerm (addbinding ctx name binding) t1
                |> Result.map (TmAbs I name resultTyT)

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
                |> Result.map (\t1t -> TmTApp I t1t (fromParseType ctx ty1))

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
