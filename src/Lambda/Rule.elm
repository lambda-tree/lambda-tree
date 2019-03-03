module Lambda.Rule exposing (..)

import Either
import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (equalTypes)
import Lambda.Parse as P
import Lambda.ParseTransform exposing (fromParseContext, fromParseTerm, fromParseType)
import Maybe exposing (..)
import Model exposing (Tree(..), TreeModel)
import Result


type TyRule
    = TVar { bottom : TypeStatement, top : TypeStatement }
    | TIf { bottom : TypeStatement, top1 : TypeStatement, top2 : TypeStatement, top3 : TypeStatement }


type alias TypeStatement =
    { ctx : Context, term : Term, ty : Ty }


type alias ContainmentStatement =
    { ctx : Context, variable : Term }


checkRule : TyRule -> Bool
checkRule rule =
    case rule of
        TVar { bottom, top } ->
            bottom.ctx
                == Debug.log "topCtx" top.ctx
                && Debug.log "bottomTerm" bottom.term
                == Debug.log "topTerm" top.term
                && Debug.log "bottomTy" bottom.ty
                == Debug.log "topTy" top.ty
                && (case bottom.term of
                        TmVar _ x _ ->
                            case getbinding top.ctx x of
                                Just (VarBind ty1) ->
                                    -- Doesn't need to be exact equal type since contexts must be identical
                                    equalTypes top.ctx ty1 bottom.ctx bottom.ty

                                _ ->
                                    False

                        _ ->
                            False
                   )

        TIf { bottom, top1, top2, top3 } ->
            case bottom.term of
                TmIf _ t1 t2 t3 ->
                    List.all ((==) bottom.ctx) [ top1.ctx, top2.ctx, top3.ctx ]
                        && (top1.term == t1)
                        && equalTypes top1.ctx top1.ty bottom.ctx (TyName "Bool")
                        && (top2.term == t2)
                        && equalTypes top2.ctx top2.ty bottom.ctx bottom.ty
                        && (top3.term == t3)
                        && equalTypes top3.ctx top3.ty bottom.ctx bottom.ty

                _ ->
                    False


getCtxTermTy : String -> String -> String -> Result String ( Context, Term, Ty )
getCtxTermTy ctx term ty =
    P.parseCtx ctx
        |> Result.andThen
            (\ctx1 ->
                P.parseTerm term
                    |> Result.andThen
                        (\term1 ->
                            P.parseType ty
                                |> Result.andThen
                                    (\ty1 ->
                                        Result.Ok
                                            ( fromParseContext ctx1
                                            , fromParseContext ctx1 |> Either.andThen (\parsedCtx -> fromParseTerm parsedCtx term1)
                                            , fromParseContext ctx1 |> Either.andThen (\parsedCtx -> fromParseType parsedCtx ty1)
                                            )
                                    )
                        )
            )
        |> Result.map (\( x, y, z ) -> Either.map3 (\a b c -> ( a, b, c )) x y z)
        |> (\r ->
                case r of
                    Result.Ok (Either.Left smt) ->
                        Result.Err "Parse Error"

                    Result.Ok (Either.Right smt) ->
                        Result.Ok smt

                    Result.Err smt ->
                        Result.Err "Parse Result Error"
           )


tryRule : TreeModel -> String
tryRule t =
    case t of
        Node { ctx, term, ty, rule } children ->
            case ( rule, children ) of
                ( Model.TVar, [ child ] ) ->
                    case child of
                        Node childC _ ->
                            let
                                bottom =
                                    getCtxTermTy ctx term ty

                                top =
                                    getCtxTermTy childC.ctx childC.term childC.ty
                            in
                            bottom
                                |> Result.andThen
                                    (\( c1, tm1, ty1 ) ->
                                        top
                                            |> Result.andThen
                                                (\( c2, tm2, ty2 ) ->
                                                    Result.Ok <|
                                                        checkRule
                                                            (TVar
                                                                { bottom =
                                                                    { ctx = c1
                                                                    , term = tm1
                                                                    , ty = ty1
                                                                    }
                                                                , top =
                                                                    { ctx = c2
                                                                    , term = tm2
                                                                    , ty = ty2
                                                                    }
                                                                }
                                                            )
                                                )
                                    )
                                |> (\r ->
                                        case r of
                                            Err text ->
                                                text

                                            Ok checks ->
                                                if checks then
                                                    "OK"

                                                else
                                                    "NOK"
                                   )

                ( Model.TIf, [ child1, child2, child3 ] ) ->
                    case ( child1, child2, child3 ) of
                        ( Node childC1 _, Node childC2 _, Node childC3 _ ) ->
                            let
                                bottom =
                                    getCtxTermTy ctx term ty

                                top1 =
                                    getCtxTermTy childC1.ctx childC1.term childC1.ty

                                top2 =
                                    getCtxTermTy childC2.ctx childC2.term childC2.ty

                                top3 =
                                    getCtxTermTy childC3.ctx childC3.term childC3.ty
                            in
                            bottom
                                |> Result.andThen
                                    (\( c1, tm1, ty1 ) ->
                                        top1
                                            |> Result.andThen
                                                (\( ctxC1, tmC1, tyC1 ) ->
                                                    top2
                                                        |> Result.andThen
                                                            (\( ctxC2, tmC2, tyC2 ) ->
                                                                top3
                                                                    |> Result.andThen
                                                                        (\( ctxC3, tmC3, tyC3 ) ->
                                                                            Result.Ok <|
                                                                                checkRule
                                                                                    (TIf
                                                                                        { bottom =
                                                                                            { ctx = c1
                                                                                            , term = tm1
                                                                                            , ty = ty1
                                                                                            }
                                                                                        , top1 =
                                                                                            { ctx = ctxC1
                                                                                            , term = tmC1
                                                                                            , ty = tyC1
                                                                                            }
                                                                                        , top2 =
                                                                                            { ctx = ctxC2
                                                                                            , term = tmC2
                                                                                            , ty = tyC2
                                                                                            }
                                                                                        , top3 =
                                                                                            { ctx = ctxC3
                                                                                            , term = tmC3
                                                                                            , ty = tyC3
                                                                                            }
                                                                                        }
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                                |> (\r ->
                                        case r of
                                            Err text ->
                                                text

                                            Ok checks ->
                                                if checks then
                                                    "OK"

                                                else
                                                    "NOK"
                                   )

                _ ->
                    ""
