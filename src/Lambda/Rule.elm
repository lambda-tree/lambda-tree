module Lambda.Rule exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (equalTypes)
import Lambda.Parse as P
import Lambda.ParseTransform exposing (ParseTransformError, fromParseContext, fromParseTerm, fromParseType)
import Maybe exposing (..)
import Model exposing (Rule, TreeModel)
import Result
import Utils.Tree exposing (Tree(..))


type TyRule
    = TVar { bottom : TypeStatement, top : TypeStatement }
    | TIf { bottom : TypeStatement, top1 : TypeStatement, top2 : TypeStatement, top3 : TypeStatement }


type alias TypeStatement =
    { ctx : Context, term : Term, ty : Ty }


type alias ContainmentStatement =
    { ctx : Context, variable : Term }


type RuleError
    = ParseError { row : Int, col : Int }
    | ParseTransformError ParseTransformError
    | PrerequisiteDataError


type alias ExprTree =
    Tree
        { ctx : Result RuleError Lambda.Context.Context
        , term : Result RuleError Lambda.Expression.Term
        , ty : Result RuleError Lambda.Expression.Ty
        , rule : Rule
        }


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


getCtxTermTy : Maybe Context -> Maybe Term -> Maybe Ty -> Maybe ( Context, Term, Ty )
getCtxTermTy ctx term ty =
    ctx
        |> Maybe.andThen
            (\ctx1 ->
                term
                    |> Maybe.andThen
                        (\term1 ->
                            ty
                                |> Maybe.andThen
                                    (\ty1 ->
                                        Maybe.Just
                                            ( ctx1
                                            , term1
                                            , ty1
                                            )
                                    )
                        )
            )


tryRule : ExprTree -> String
tryRule t =
    case t of
        Node { ctx, term, ty, rule } children ->
            case ( rule, children ) of
                ( Model.TVar, [ child ] ) ->
                    case child of
                        Node childC _ ->
                            let
                                bottom =
                                    getCtxTermTy (Result.toMaybe ctx) (Result.toMaybe term) (Result.toMaybe ty)

                                top =
                                    getCtxTermTy (Result.toMaybe childC.ctx) (Result.toMaybe childC.term) (Result.toMaybe childC.ty)
                            in
                            bottom
                                |> Maybe.andThen
                                    (\( c1, tm1, ty1 ) ->
                                        top
                                            |> Maybe.andThen
                                                (\( c2, tm2, ty2 ) ->
                                                    Maybe.Just <|
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
                                            Nothing ->
                                                "NOTHING"

                                            Just checks ->
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
                                    getCtxTermTy (Result.toMaybe ctx) (Result.toMaybe term) (Result.toMaybe ty)

                                top1 =
                                    getCtxTermTy (Result.toMaybe childC1.ctx) (Result.toMaybe childC1.term) (Result.toMaybe childC1.ty)

                                top2 =
                                    getCtxTermTy (Result.toMaybe childC2.ctx) (Result.toMaybe childC2.term) (Result.toMaybe childC2.ty)

                                top3 =
                                    getCtxTermTy (Result.toMaybe childC3.ctx) (Result.toMaybe childC3.term) (Result.toMaybe childC3.ty)
                            in
                            bottom
                                |> Maybe.andThen
                                    (\( c1, tm1, ty1 ) ->
                                        top1
                                            |> Maybe.andThen
                                                (\( ctxC1, tmC1, tyC1 ) ->
                                                    top2
                                                        |> Maybe.andThen
                                                            (\( ctxC2, tmC2, tyC2 ) ->
                                                                top3
                                                                    |> Maybe.andThen
                                                                        (\( ctxC3, tmC3, tyC3 ) ->
                                                                            Maybe.Just <|
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
                                            Nothing ->
                                                "NOTHING"

                                            Just checks ->
                                                if checks then
                                                    "OK"

                                                else
                                                    "NOK"
                                   )

                _ ->
                    ""
