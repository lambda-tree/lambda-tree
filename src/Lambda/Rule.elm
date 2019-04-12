module Lambda.Rule exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (degeneralizeTypeTop, equalTypes, generalizeTypeTop, isSpecializedType, typeSubstTop)
import Lambda.ParseTransform exposing (ParseTransformError)
import Maybe exposing (..)
import Model exposing (Rule, TreeModel)
import Result
import Result.Extra
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
    | TLet { bottom : TypeStatement, top1 : TypeStatement, top2 : TypeStatement }
    | TGen { bottom : TypeStatement, top : TypeStatement }
    | TInst { bottom : TypeStatement, top : TypeStatement }


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


checkRule : TyRule -> Result String ()
checkRule rule =
    case rule of
        TVar { bottom, top } ->
            Ok ()
                |> check ( "ctxSame", bottom.ctx == top.ctx )
                |> check ( "termSame", bottom.term == top.term )
                |> check ( "tySame", bottom.ty == top.ty )
                |> check
                    ( "types of vars are same"
                    , case bottom.term of
                        TmVar _ x _ ->
                            case getbinding top.ctx x of
                                Just (VarBind ty1) ->
                                    equalTypes
                                        (List.drop (x + 1) top.ctx)
                                        ty1
                                        bottom.ctx
                                        bottom.ty

                                _ ->
                                    False

                        _ ->
                            False
                    )

        TIf { bottom, top1, top2, top3 } ->
            case bottom.term of
                TmIf _ t1 t2 t3 ->
                    Ok ()
                        |> check ( "allCtxSame", List.all ((==) bottom.ctx) [ top1.ctx, top2.ctx, top3.ctx ] )
                        |> check ( "ifTermSame", top1.term == t1 )
                        |> check ( "ifTypeIsBool", equalTypes top1.ctx top1.ty bottom.ctx (TyConst TyBool) )
                        |> check ( "thenTermSame", top2.term == t2 )
                        |> check ( "thenTypeSame", equalTypes top2.ctx top2.ty bottom.ctx bottom.ty )
                        |> check ( "elseTermSame", top3.term == t3 )
                        |> check ( "elseTypeSame", equalTypes top3.ctx top3.ty bottom.ctx bottom.ty )

                _ ->
                    Err "wrongRule"

        TTrue { bottom, top } ->
            case bottom.term of
                TmConst _ TmTrue ->
                    Ok ()
                        |> check ( "ctxSame", bottom.ctx == top.ctx )
                        |> check ( "termSame", top.term == bottom.term )
                        |> check ( "typeIsBool", equalTypes top.ctx top.ty bottom.ctx (TyConst TyBool) )

                _ ->
                    Err "wrongRule"

        TFalse { bottom, top } ->
            case bottom.term of
                TmConst _ TmFalse ->
                    Ok ()
                        |> check ( "ctxSame", bottom.ctx == top.ctx )
                        |> check ( "termSame", top.term == bottom.term )
                        |> check ( "typeIsBool", equalTypes top.ctx top.ty bottom.ctx (TyConst TyBool) )

                _ ->
                    Err "wrongRule"

        TAbs { bottom, top } ->
            case ( bottom.term, bottom.ty ) of
                ( TmAbs _ varName ty t, TyArr ty1 ty2 ) ->
                    Ok ()
                        |> check ( "ctxSame", popbinding top.ctx == bottom.ctx )
                        |> check ( "varTypeInCtx", top.ctx == addbinding bottom.ctx varName (VarBind ty1) )
                        |> check ( "termSame", top.term == t )
                        |> check ( "returnTypeRightSameAsTop", equalTypes top.ctx top.ty bottom.ctx ty2 )
                        |> check
                            ( "returnTypeLeftSameAsArg"
                            , ty
                                |> Maybe.map (\justTy -> equalTypes bottom.ctx justTy bottom.ctx ty1)
                                |> Maybe.withDefault True
                            )

                _ ->
                    Err "wrongRule"

        TApp { bottom, top1, top2 } ->
            case ( bottom.term, top1.ty ) of
                ( TmApp _ t1 t2, TyArr ty1 ty2 ) ->
                    Ok ()
                        |> check ( "allCtxSame", List.all ((==) bottom.ctx) [ top1.ctx, top2.ctx ] )
                        |> check ( "leftTermSame", top1.term == t1 )
                        |> check ( "rightTermSame", top2.term == t2 )
                        |> check ( "leftArrowTypeSameAsArgType", equalTypes top1.ctx ty1 top2.ctx top2.ty )
                        |> check ( "rightArrowTypeSameAsAppResultType", equalTypes top1.ctx ty2 bottom.ctx bottom.ty )

                _ ->
                    Err "wrongRule"

        TTAbs { bottom, top } ->
            case ( bottom.term, bottom.ty ) of
                ( TmTAbs _ tyVarName1 t, TyAll tyVarName2 ty ) ->
                    Ok ()
                        |> check ( "typeVariablesSame", tyVarName1 == tyVarName2 )
                        |> check ( "isAddedToContext", addbinding bottom.ctx tyVarName1 TyVarBind == top.ctx )
                        |> check ( "isFree", not <| isnamebound bottom.ctx tyVarName1 )
                        |> check ( "termsSame", top.term == t )
                        |> check ( "typesSame", ty == top.ty )

                _ ->
                    Err "wrongRule"

        TTApp { bottom, top } ->
            case bottom.term of
                TmTApp _ t ty2 ->
                    case top.ty of
                        TyAll _ ty1 ->
                            Ok ()
                                |> check ( "ctxSame", bottom.ctx == top.ctx )
                                |> check ( "termSame", top.term == t )
                                |> check ( "typeSubstitutionCorrect", bottom.ty == typeSubstTop ty2 ty1 )

                        _ ->
                            Err "topTypeNotForAll"

                _ ->
                    Err "wrongRule"

        TLet { bottom, top1, top2 } ->
            case bottom.term of
                TmLet _ varName t1 t2 ->
                    Ok ()
                        |> check ( "bottom & top1 ctxs are same", bottom.ctx == top1.ctx )
                        |> check ( "variable is added to ctx with correct type binding", top2.ctx == addbinding bottom.ctx varName (VarBind top1.ty) )
                        |> check ( "in expr terms are same", t2 == top2.term )
                        |> check ( "in expr types are same", bottom.ty == top2.ty )
                        |> check ( "bound terms are same", t1 == top1.term )

                _ ->
                    Err "wrongRule"

        TGen { bottom, top } ->
            case bottom.ty of
                TyAll varName _ ->
                    Ok ()
                        |> check ( "ctxs are same", bottom.ctx == top.ctx )
                        |> check ( "terms are same", bottom.term == top.term )
                        |> check ( "var is not a free var of ctx", isnamebound bottom.ctx varName |> not )
                        |> check ( "type is degeneralized", degeneralizeTypeTop bottom.ctx bottom.ty == top.ty )
                        |> check ( "type is generalized", generalizeTypeTop top.ctx top.ty varName == bottom.ty )

                _ ->
                    Err "wrongRule"

        TInst { bottom, top } ->
            Ok ()
                |> check ( "ctxs are same", bottom.ctx == top.ctx )
                |> check ( "terms are same", bottom.term == top.term )
                |> check ( "type is subtype", isSpecializedType top.ctx top.ty bottom.ty == Ok True )


check : ( String, Bool ) -> Result String () -> Result String ()
check condition previous =
    previous
        |> Result.andThen
            (\_ ->
                case condition of
                    ( error, conditionResult ) ->
                        let
                            _ =
                                Debug.log ("CHECK: " ++ error) conditionResult
                        in
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

        getResultMsg : Result String () -> String
        getResultMsg =
            Result.map (\() -> "OK") >> Result.Extra.merge
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
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

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
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

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
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

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
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

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
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

                Model.TTAbs ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TTAbs
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
                                    , top =
                                        { ctx = c1.ctx
                                        , term = c1.term
                                        , ty = c1.ty
                                        }
                                    }
                                )
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

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
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

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
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

                Model.TLet ->
                    case children of
                        [ Node (Result.Ok c1) _, Node (Result.Ok c2) _ ] ->
                            checkRule
                                (TLet
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
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

                Model.TGen ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TGen
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
                                    , top =
                                        { ctx = c1.ctx
                                        , term = c1.term
                                        , ty = c1.ty
                                        }
                                    }
                                )
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

                Model.TInst ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TInst
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
                                    , top =
                                        { ctx = c1.ctx
                                        , term = c1.term
                                        , ty = c1.ty
                                        }
                                    }
                                )
                                |> getResultMsg

                        _ ->
                            "Top rule parse Error"

                _ ->
                    "Unimplemented Rule"

        _ ->
            "Bottom rule error"
