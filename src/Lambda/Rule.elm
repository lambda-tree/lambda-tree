module Lambda.Rule exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (areHMTypesEquivalent, degeneralizeTypeTop, equalTypes, gen, generalizeTypeTop, isSpecializedType, typeSubstTop)
import Lambda.ParseTransform exposing (ParseTransformError)
import List.Extra
import Maybe exposing (..)
import Result
import Utils.Tree exposing (Tree(..))


type Rule
    = TTrue
    | TFalse
    | TVar
    | TVarInst
    | TAbs
    | TApp
    | TIf
    | TTAbs
    | TTApp
    | TLet
    | TLetGen
    | TGen
    | TInst
    | NoRule


type TyRule
    = TyRuleTVar { bottom : TypeStatement, top : TypeStatement }
    | TyRuleTVarInst { bottom : TypeStatement, top : TypeStatement }
    | TyRuleTIf { bottom : TypeStatement, top1 : TypeStatement, top2 : TypeStatement, top3 : TypeStatement }
    | TyRuleTTrue { bottom : TypeStatement, top : TypeStatement }
    | TyRuleTFalse { bottom : TypeStatement, top : TypeStatement }
    | TyRuleTAbs { bottom : TypeStatement, top : TypeStatement }
    | TyRuleTApp { bottom : TypeStatement, top1 : TypeStatement, top2 : TypeStatement }
    | TyRuleTTAbs { bottom : TypeStatement, top : TypeStatement }
    | TyRuleTTApp { bottom : TypeStatement, top : TypeStatement }
    | TyRuleTLet { bottom : TypeStatement, top1 : TypeStatement, top2 : TypeStatement }
    | TyRuleTLetGen { bottom : TypeStatement, top1 : TypeStatement, top2 : TypeStatement }
    | TyRuleTGen { bottom : TypeStatement, top : TypeStatement }
    | TyRuleTInst { bottom : TypeStatement, top : TypeStatement }


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
        TyRuleTVar { bottom, top } ->
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

        TyRuleTVarInst { bottom, top } ->
            Ok ()
                |> check ( "Contexts are not same", bottom.ctx == top.ctx )
                |> check ( "Terms are not same", bottom.term == top.term )
                |> check ( "Types are not same", bottom.ty == top.ty )
                |> checkWithDetails
                    ( "Type of var is incorrectly specialized"
                    , case bottom.term of
                        TmVar _ x _ ->
                            case getbinding top.ctx x of
                                Just (VarBind ty1) ->
                                    isSpecializedType top.ctx
                                        ty1
                                        bottom.ty

                                _ ->
                                    Err "Variable is not bound in context"

                        _ ->
                            Ok <| False
                    )

        TyRuleTIf { bottom, top1, top2, top3 } ->
            case bottom.term of
                TmIf _ t1 t2 t3 ->
                    Ok ()
                        |> check ( "Contexts are not same", List.all ((==) bottom.ctx) [ top1.ctx, top2.ctx, top3.ctx ] )
                        |> check ( "'If' part of term is not same", top1.term == t1 )
                        |> check ( "'If' part of term has not type of Bool", equalTypes top1.ctx top1.ty bottom.ctx (TyConst TyBool) )
                        |> check ( "'Then' part of term is not same", top2.term == t2 )
                        |> check ( "Type of 'then' part is not same", equalTypes top2.ctx top2.ty bottom.ctx bottom.ty )
                        |> check ( "'Else' part of term is not same", top3.term == t3 )
                        |> check ( "Type of 'else' part is not same", equalTypes top3.ctx top3.ty bottom.ctx bottom.ty )

                _ ->
                    Err "wrongRule"

        TyRuleTTrue { bottom, top } ->
            case bottom.term of
                TmConst _ TmTrue ->
                    Ok ()
                        |> check ( "ctxSame", bottom.ctx == top.ctx )
                        |> check ( "termSame", top.term == bottom.term )
                        |> check ( "typeIsBool", equalTypes top.ctx top.ty bottom.ctx (TyConst TyBool) )

                _ ->
                    Err "wrongRule"

        TyRuleTFalse { bottom, top } ->
            case bottom.term of
                TmConst _ TmFalse ->
                    Ok ()
                        |> check ( "ctxSame", bottom.ctx == top.ctx )
                        |> check ( "termSame", top.term == bottom.term )
                        |> check ( "typeIsBool", equalTypes top.ctx top.ty bottom.ctx (TyConst TyBool) )

                _ ->
                    Err "wrongRule"

        TyRuleTAbs { bottom, top } ->
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

        TyRuleTApp { bottom, top1, top2 } ->
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

        TyRuleTTAbs { bottom, top } ->
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

        TyRuleTTApp { bottom, top } ->
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

        TyRuleTLet { bottom, top1, top2 } ->
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

        TyRuleTLetGen { bottom, top1, top2 } ->
            case bottom.term of
                TmLet _ varName t1 t2 ->
                    Ok ()
                        |> check ( "bottom & top1 ctxs are same", bottom.ctx == top1.ctx )
                        |> checkWithDetails
                            ( "variable is added to ctx with correct type binding"
                            , let
                                genTy =
                                    gen top1.ctx top1.ty
                              in
                              case List.Extra.uncons top2.ctx of
                                Just ( ( boundVarName, binding ), rest ) ->
                                    case binding of
                                        VarBind boundTy ->
                                            Ok ()
                                                |> check ( "Context is different", rest == bottom.ctx )
                                                |> check ( "Var is added to ctx with different name", boundVarName == varName )
                                                |> checkWithDetails
                                                    ( "Var added to ctx is not generalized correctly"
                                                    , areHMTypesEquivalent top1.ctx boundTy genTy
                                                        |> Result.map (\_ -> True)
                                                    )
                                                |> Result.map (\_ -> True)

                                        _ ->
                                            Err "Type of variable added to context is missing"

                                _ ->
                                    Err "Context is empty"
                            )
                        |> check ( "in expr terms are same", t2 == top2.term )
                        |> check ( "in expr types are same", bottom.ty == top2.ty )
                        |> check ( "bound terms are same", t1 == top1.term )

                _ ->
                    Err "wrongRule"

        TyRuleTGen { bottom, top } ->
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

        TyRuleTInst { bottom, top } ->
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


checkWithDetails : ( String, Result String Bool ) -> Result String () -> Result String ()
checkWithDetails condition previous =
    previous
        |> Result.andThen
            (\_ ->
                case condition of
                    ( error, conditionResult ) ->
                        let
                            _ =
                                Debug.log ("CHECK: " ++ error) conditionResult
                        in
                        case conditionResult of
                            Ok True ->
                                Ok ()

                            Err e ->
                                Err <| error ++ ": " ++ e

                            Ok False ->
                                Err error
            )


type alias ResolvedExprTree =
    Tree
        (Result String
            { ctx : Lambda.Context.Context
            , term : Lambda.Expression.Term
            , ty : Lambda.Expression.Ty
            , rule : Rule
            }
        )


tryRule : ExprTree -> Result String ()
tryRule t =
    let
        erasedErrorTree : ResolvedExprTree
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
                TVar ->
                    case r.term of
                        TmVar _ _ _ ->
                            case children of
                                [ Node (Result.Ok c1) _ ] ->
                                    checkRule
                                        (TyRuleTVar
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

                                _ ->
                                    Err "Top rule parse Error"

                        _ ->
                            Err "wrongRule"

                TVarInst ->
                    case r.term of
                        TmVar _ _ _ ->
                            case children of
                                [ Node (Result.Ok c1) _ ] ->
                                    checkRule
                                        (TyRuleTVarInst
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

                                _ ->
                                    Err "Top rule parse Error"

                        _ ->
                            Err "wrongRule"

                TIf ->
                    case children of
                        [ Node (Result.Ok c1) _, Node (Result.Ok c2) _, Node (Result.Ok c3) _ ] ->
                            checkRule
                                (TyRuleTIf
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

                        _ ->
                            Err "Top rule parse Error"

                TTrue ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TyRuleTTrue
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

                        _ ->
                            Err "Top rule parse Error"

                TFalse ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TyRuleTTrue
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

                        _ ->
                            Err "Top rule parse Error"

                TAbs ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TyRuleTAbs
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
                                    , top =
                                        { ctx = c1.ctx
                                        , term = c1.term
                                        , ty = c1.ty
                                        }
                                    }
                                )

                        _ ->
                            Err "Top rule parse Error"

                TTAbs ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TyRuleTTAbs
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
                                    , top =
                                        { ctx = c1.ctx
                                        , term = c1.term
                                        , ty = c1.ty
                                        }
                                    }
                                )

                        _ ->
                            Err "Top rule parse Error"

                TApp ->
                    case children of
                        [ Node (Result.Ok c1) _, Node (Result.Ok c2) _ ] ->
                            checkRule
                                (TyRuleTApp
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

                        _ ->
                            Err "Top rule parse Error"

                TTApp ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TyRuleTTApp
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
                                    , top =
                                        { ctx = c1.ctx
                                        , term = c1.term
                                        , ty = c1.ty
                                        }
                                    }
                                )

                        _ ->
                            Err "Top rule parse Error"

                TLet ->
                    case children of
                        [ Node (Result.Ok c1) _, Node (Result.Ok c2) _ ] ->
                            checkRule
                                (TyRuleTLet
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

                        _ ->
                            Err "Top rule parse Error"

                TLetGen ->
                    case children of
                        [ Node (Result.Ok c1) _, Node (Result.Ok c2) _ ] ->
                            checkRule
                                (TyRuleTLetGen
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

                        _ ->
                            Err "Top rule parse Error"

                TGen ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TyRuleTGen
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
                                    , top =
                                        { ctx = c1.ctx
                                        , term = c1.term
                                        , ty = c1.ty
                                        }
                                    }
                                )

                        _ ->
                            Err "Top rule parse Error"

                TInst ->
                    case children of
                        [ Node (Result.Ok c1) _ ] ->
                            checkRule
                                (TyRuleTInst
                                    { bottom = { ctx = r.ctx, term = r.term, ty = r.ty }
                                    , top =
                                        { ctx = c1.ctx
                                        , term = c1.term
                                        , ty = c1.ty
                                        }
                                    }
                                )

                        _ ->
                            Err "Top rule parse Error"

                _ ->
                    Err "Unimplemented Rule"

        _ ->
            Err "Bottom expression error"


rulesForTypeSystem : TypeSystem -> List Rule
rulesForTypeSystem ts =
    case ts of
        SimplyTyped ->
            [ TTrue
            , TFalse
            , TVar
            , TAbs
            , TApp
            , TIf
            , TLet
            ]

        HM NonDeterministic ->
            [ TTrue
            , TFalse
            , TVar
            , TAbs
            , TApp
            , TIf
            , TLet
            , TGen
            , TInst
            ]

        HM SyntaxDirected ->
            [ TTrue
            , TFalse
            , TVarInst
            , TAbs
            , TApp
            , TIf
            , TLetGen
            ]

        SystemF ->
            [ TTrue
            , TFalse
            , TVar
            , TAbs
            , TApp
            , TTAbs
            , TTApp
            , TIf
            , TLet
            ]


isTerminalRule : Rule -> Bool
isTerminalRule rule =
    case rule of
        TTrue ->
            True

        TFalse ->
            True

        TVar ->
            True

        TVarInst ->
            True

        TAbs ->
            False

        TApp ->
            False

        TIf ->
            False

        TTAbs ->
            False

        TTApp ->
            False

        TLet ->
            False

        TLetGen ->
            False

        TGen ->
            False

        TInst ->
            False

        NoRule ->
            False
