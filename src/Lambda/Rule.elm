module Lambda.Rule exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (equalTypes, typeSubstTop)
import Lambda.ParseTransform exposing (ParseTransformError)
import Maybe exposing (..)
import Model exposing (Rule, TreeModel)
import Result
import Utils.Tree exposing (Tree(..))


type TyRule
    = TVar { bottom : TypeStatement, top : TypeStatement }
    | TIf { bottom : TypeStatement, top1 : TypeStatement, top2 : TypeStatement, top3 : TypeStatement }
    | TTrue { bottom : TypeStatement, top : TypeStatement }
    | TFalse { bottom : TypeStatement, top : TypeStatement }
    | TAbs { bottom : TypeStatement, top : TypeStatement }
    | TApp { bottom : TypeStatement, top1 : TypeStatement, top2 : TypeStatement }
    | TTAbs { bottom : TypeStatement, top : TypeStatement }
    | TTApp { bottom : TypeStatement, top : TypeStatement }


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
            (bottom.ctx == top.ctx)
                && (bottom.term == top.term)
                && (bottom.ty == top.ty)
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

        TTrue { bottom, top } ->
            case bottom.term of
                TmConst _ TmTrue ->
                    (bottom.ctx == top.ctx)
                        && (top.term == bottom.term)
                        && equalTypes top.ctx top.ty bottom.ctx (TyName "Bool")

                _ ->
                    False

        TFalse { bottom, top } ->
            case bottom.term of
                TmConst _ TmFalse ->
                    (bottom.ctx == top.ctx)
                        && (top.term == bottom.term)
                        && equalTypes top.ctx top.ty bottom.ctx (TyName "Bool")

                _ ->
                    False

        TAbs { bottom, top } ->
            case ( bottom.term, bottom.ty ) of
                ( TmAbs _ varName ty t, TyArr ty1 ty2 ) ->
                    (popbinding top.ctx == bottom.ctx)
                        && (top.ctx == addbinding bottom.ctx varName (VarBind ty))
                        && (top.term == t)
                        && equalTypes top.ctx top.ty bottom.ctx ty2
                        && equalTypes bottom.ctx ty bottom.ctx ty1

                _ ->
                    False

        TApp { bottom, top1, top2 } ->
            case ( bottom.term, top1.ty ) of
                ( TmApp _ t1 t2, TyArr ty1 ty2 ) ->
                    List.all ((==) bottom.ctx) [ top1.ctx, top2.ctx ]
                        && (top1.term == t1)
                        && (top2.term == t2)
                        && equalTypes top1.ctx ty1 top2.ctx top2.ty
                        && equalTypes top1.ctx ty2 bottom.ctx bottom.ty

                _ ->
                    False

        TTAbs { bottom, top } ->
            case ( bottom.term, bottom.ty ) of
                ( TmTAbs _ tyVarName1 t, TyAll tyVarName2 ty ) ->
                    Ok ()
                        |> check ( "1 - typeVariablesSame", tyVarName1 == tyVarName2 )
                        |> check ( "2 - isAddedToContext", addbinding bottom.ctx tyVarName1 TyVarBind == top.ctx )
                        |> check ( "3 - isFree", not <| isnamebound bottom.ctx tyVarName1 )
                        |> check ( "4 - termsEqual", top.term == t )
                        |> check ( "5 - typesEqual", ty == top.ty )
                        |> Result.map (\_ -> True)
                        |> Result.withDefault False

                _ ->
                    False

        TTApp { bottom, top } ->
            case ( bottom.term, top.ty ) of
                ( TmTApp _ t ty2, TyAll _ ty1 ) ->
                    (bottom.ctx == top.ctx)
                        && (top.term == t)
                        && (bottom.ty == typeSubstTop ty2 ty1)

                _ ->
                    False


check : ( String, Bool ) -> Result String () -> Result String ()
check condition previous =
    previous
        |> Result.andThen
            (\_ ->
                case condition of
                    ( error, conditionResult ) ->
                        if conditionResult then
                            Ok ()

                        else
                            Err error
            )


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

                Model.TTrue ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TTrue
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

                Model.TFalse ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TTrue
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

                Model.TAbs ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TAbs
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
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

                Model.TApp ->
                    case children of
                        [ Node (Result.Ok c1) _, Node (Result.Ok c2) _ ] ->
                            checkRule
                                (TApp
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
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

                Model.TTApp ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TTApp
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
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

                _ ->
                    "Unimplemented Rule"

        _ ->
            "Bottom rule error"
