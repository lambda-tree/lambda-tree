module Lambda.Rule exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (equalTypes)
import Lambda.ParseTransform exposing (ParseTransformError)
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


tryRule : ExprTree -> String
tryRule t =
    let
        erasedErrorTree =
            t
                |> Utils.Tree.map
                    (\{ ctx, term, ty, rule } ->
                        case ( ctx, term, ty ) of
                            ( Result.Ok ctx_, Result.Ok term_, Result.Ok ty_ ) ->
                                Result.Ok
                                    { ctx = ctx_
                                    , term = term_
                                    , ty = ty_
                                    , rule = rule
                                    }

                            _ ->
                                Result.Err "Field is missing"
                    )
    in
    case erasedErrorTree of
        Node (Result.Ok r) children ->
            case r.rule of
                Model.TVar ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TVar
                                    { bottom =
                                        { ctx = r.ctx
                                        , term = r.term
                                        , ty = r.ty
                                        }
                                    , top =
                                        { ctx = c1.ctx
                                        , term = c1.term
                                        , ty = c1.ty
                                        }
                                    }
                                )
                                |> (\checks ->
                                        if checks then
                                            "OK"

                                        else
                                            "NOK"
                                   )

                        _ ->
                            "Top rule Error"

                Model.TIf ->
                    case children of
                        [ Node (Result.Ok c1) _, Node (Result.Ok c2) _, Node (Result.Ok c3) _ ] ->
                            checkRule
                                (TIf
                                    { bottom =
                                        { ctx = r.ctx
                                        , term = r.term
                                        , ty = r.ty
                                        }
                                    , top1 =
                                        { ctx = c1.ctx
                                        , term = c1.term
                                        , ty = c1.ty
                                        }
                                    , top2 =
                                        { ctx = c2.ctx
                                        , term = c2.term
                                        , ty = c2.ty
                                        }
                                    , top3 =
                                        { ctx = c3.ctx
                                        , term = c3.term
                                        , ty = c3.ty
                                        }
                                    }
                                )
                                |> (\checks ->
                                        if checks then
                                            "OK"

                                        else
                                            "NOK"
                                   )

                        _ ->
                            "Top rule Error"

                _ ->
                    "Unimplemented Rule"

        _ ->
            "Bottom rule error"
