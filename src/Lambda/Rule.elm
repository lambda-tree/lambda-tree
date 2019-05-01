module Lambda.Rule exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (areHMTypesEquivalent, degeneralizeTermTop, degeneralizeTypeTop, equalTypes, ftvCtx, ftvTy, gen, generalizeTypeTop, isSpecializedType, typeSubstTop)
import Lambda.ParseTransform exposing (ParseTransformError)
import Lambda.RuleError
import List.Extra
import Maybe exposing (..)
import Result
import Set
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


type ExprError
    = ParseError { row : Int, col : Int }
    | ParseTransformError ParseTransformError
    | PrerequisiteDataError
    | NotInTypeSystemError
    | EmptyTypeError


type alias ExprTreeContent =
    { ctx : Result ExprError Lambda.Context.Context
    , term : Result ExprError Lambda.Expression.Term
    , ty : Result ExprError Lambda.Expression.Ty
    , rule : Rule
    }


type alias ExprTree =
    Tree ExprTreeContent


checkRule : TyRule -> Result String ()
checkRule rule =
    case rule of
        TyRuleTVar { bottom, top } ->
            case bottom.term of
                TmVar _ _ _ ->
                    Ok ()
                        |> check ( Lambda.RuleError.ctxSame, bottom.ctx == top.ctx )
                        |> check ( Lambda.RuleError.termSame, bottom.term == top.term )
                        |> check ( Lambda.RuleError.tySame, bottom.ty == top.ty )
                        |> check
                            ( Lambda.RuleError.varTyCtx
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

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTVarInst { bottom, top } ->
            case bottom.term of
                TmVar _ _ _ ->
                    Ok ()
                        |> check ( Lambda.RuleError.ctxSame, bottom.ctx == top.ctx )
                        |> check ( Lambda.RuleError.termSame, bottom.term == top.term )
                        |> check ( Lambda.RuleError.tySame, bottom.ty == top.ty )
                        |> checkWithDetails
                            ( Lambda.RuleError.varSpec
                            , case bottom.term of
                                TmVar _ x _ ->
                                    case getbinding top.ctx x of
                                        Just (VarBind ty1) ->
                                            isSpecializedType top.ctx
                                                ty1
                                                bottom.ty

                                        _ ->
                                            Err Lambda.RuleError.varNotInCtx

                                _ ->
                                    Ok <| False
                            )

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTIf { bottom, top1, top2, top3 } ->
            case bottom.term of
                TmIf _ t1 t2 t3 ->
                    Ok ()
                        |> check ( Lambda.RuleError.ctxSame, List.all ((==) bottom.ctx) [ top1.ctx, top2.ctx, top3.ctx ] )
                        |> check ( "'If' part of term is not same", top1.term == t1 )
                        |> check ( "'If' part of term has not type of Bool", equalTypes top1.ctx top1.ty bottom.ctx (TyConst TyBool) )
                        |> check ( "'Then' part of term is not same", top2.term == t2 )
                        |> check ( "Type of 'then' part is not same", equalTypes top2.ctx top2.ty bottom.ctx bottom.ty )
                        |> check ( "'Else' part of term is not same", top3.term == t3 )
                        |> check ( "Type of 'else' part is not same", equalTypes top3.ctx top3.ty bottom.ctx bottom.ty )

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTTrue { bottom, top } ->
            case bottom.term of
                TmConst _ TmTrue ->
                    Ok ()
                        |> check ( Lambda.RuleError.ctxSame, bottom.ctx == top.ctx )
                        |> check ( Lambda.RuleError.termSame, top.term == bottom.term )
                        |> check ( Lambda.RuleError.tyBool, equalTypes top.ctx top.ty bottom.ctx (TyConst TyBool) )

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTFalse { bottom, top } ->
            case bottom.term of
                TmConst _ TmFalse ->
                    Ok ()
                        |> check ( Lambda.RuleError.ctxSame, bottom.ctx == top.ctx )
                        |> check ( Lambda.RuleError.termSame, top.term == bottom.term )
                        |> check ( Lambda.RuleError.tyBool, equalTypes top.ctx top.ty bottom.ctx (TyConst TyBool) )

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTAbs { bottom, top } ->
            case bottom.term of
                TmAbs _ varName ty t ->
                    case bottom.ty of
                        TyArr ty1 ty2 ->
                            Ok ()
                                |> check ( "Top context doesn't contain whole bottom context", popbinding top.ctx == bottom.ctx || top.ctx == bottom.ctx )
                                |> check ( "Variable is not added to context with correct type", top.ctx == addbinding bottom.ctx varName (VarBind ty1) )
                                |> check ( "Top expression is not same as the body of the abstraction", top.term == t )
                                |> check ( "Return type of the abstraction is not same as type of the top expression", equalTypes top.ctx top.ty bottom.ctx ty2 )
                                |> check
                                    ( "Abstraction argument type is not same in bottom term & type"
                                      -- TODO: differentiate between type systems
                                    , ty
                                        |> Maybe.map (\justTy -> equalTypes bottom.ctx justTy bottom.ctx ty1)
                                        |> Maybe.withDefault True
                                    )

                        _ ->
                            Err "Type of abstraction is not in form σ → τ"

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTApp { bottom, top1, top2 } ->
            case bottom.term of
                TmApp _ t1 t2 ->
                    case top1.ty of
                        TyArr ty1 ty2 ->
                            Ok ()
                                |> check ( Lambda.RuleError.ctxSame, List.all ((==) bottom.ctx) [ top1.ctx, top2.ctx ] )
                                |> check ( "Abstraction part of term is not same", top1.term == t1 )
                                |> check ( "Argument part of term is not same", top2.term == t2 )
                                |> check ( "Abstraction argument type in top left premise is not same as type of the argument in top right premise", equalTypes top1.ctx ty1 top2.ctx top2.ty )
                                |> check ( "Abstraction return type in top left premise is not same as type of application", equalTypes top1.ctx ty2 bottom.ctx bottom.ty )

                        _ ->
                            Err "Type of abstraction is not in form σ → τ"

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTTAbs { bottom, top } ->
            case bottom.term of
                TmTAbs _ tyVarName1 t ->
                    case bottom.ty of
                        TyAll tyVarName2 ty ->
                            Ok ()
                                |> check ( "Type variables in term and type are not same", tyVarName1 == tyVarName2 )
                                |> check ( Lambda.RuleError.ctxSame, bottom.ctx == top.ctx )
                                -- TODO: Check for free variables
                                |> check
                                    ( "Type abstraction variable is in free variables of context"
                                    , not <| Set.member tyVarName1 (Set.union (ftvTy bottom.ty) (ftvCtx bottom.ctx))
                                    )
                                |> check ( "Type abstraction body part of term is not same", degeneralizeTermTop bottom.ctx bottom.term == top.term )
                                |> check ( "The quantified part of type is not same", degeneralizeTypeTop bottom.ctx bottom.ty == top.ty )

                        _ ->
                            Err "Type of type abstraction is not in form ∀.ɑ σ"

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTTApp { bottom, top } ->
            case bottom.term of
                TmTApp _ t ty2 ->
                    case top.ty of
                        TyAll _ ty1 ->
                            Ok ()
                                |> check ( Lambda.RuleError.ctxSame, bottom.ctx == top.ctx )
                                |> check ( "The abstraction part of the term is not the same", top.term == t )
                                |> check ( "Type is not substituted correctly for the type variable", bottom.ty == typeSubstTop ty2 ty1 )

                        _ ->
                            Err "Type of abstraction part is not universally quantified with type variable"

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTLet { bottom, top1, top2 } ->
            case bottom.term of
                TmLet _ varName t1 t2 ->
                    Ok ()
                        |> check ( "Contexts of binding part are not same", bottom.ctx == top1.ctx )
                        |> check ( "Variable is not added to ctx with correct type", top2.ctx == addbinding bottom.ctx varName (VarBind top1.ty) )
                        |> check ( "Terms of 'in' expression are not same", t2 == top2.term )
                        |> check ( "Types of 'in' expression are not same", bottom.ty == top2.ty )
                        |> check ( "Terms of the binding part are not same", t1 == top1.term )

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTLetGen { bottom, top1, top2 } ->
            case bottom.term of
                TmLet _ varName t1 t2 ->
                    Ok ()
                        |> check ( "Contexts of binding part are not same", bottom.ctx == top1.ctx )
                        |> checkWithDetails
                            ( "Variable is not added to ctx with correct type"
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
                        |> check ( "Terms of 'in' expression are not same", t2 == top2.term )
                        |> check ( "Types of 'in' expression are not same", bottom.ty == top2.ty )
                        |> check ( "Terms of the binding part are not same", t1 == top1.term )

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TyRuleTGen { bottom, top } ->
            case bottom.ty of
                TyAll varName _ ->
                    Ok ()
                        |> check ( Lambda.RuleError.ctxSame, bottom.ctx == top.ctx )
                        |> check ( Lambda.RuleError.termSame, bottom.term == top.term )
                        |> check
                            ( "Quantified type variable is in free variables of context"
                            , not <|
                                Set.member varName (Set.union (ftvTy bottom.ty) (ftvCtx bottom.ctx))
                            )
                        |> check ( "Type is incorrectly generalized", degeneralizeTypeTop bottom.ctx bottom.ty == top.ty )
                        |> check ( "Type is incorrectly generalized", generalizeTypeTop top.ctx top.ty varName == bottom.ty )

                _ ->
                    Err Lambda.RuleError.wrongRuleType

        TyRuleTInst { bottom, top } ->
            Ok ()
                |> check ( Lambda.RuleError.ctxSame, bottom.ctx == top.ctx )
                |> check ( Lambda.RuleError.termSame, bottom.term == top.term )
                |> check ( "Type is not a subtype", isSpecializedType top.ctx top.ty bottom.ty == Ok True )


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


tryRule : TypeSystem -> ExprTree -> Result String ()
tryRule typeSystem tree =
    let
        erasedErrorTree : ResolvedExprTree
        erasedErrorTree =
            tree
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
            canApplyRuleInTypeSystem typeSystem r.rule
                |> Result.andThen (\() -> canApplyRuleToExpr r)
                |> Result.andThen
                    (\() ->
                        case r.rule of
                            TVar ->
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
                                        Err Lambda.RuleError.topRuleParse

                            TVarInst ->
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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

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
                                        Err Lambda.RuleError.topRuleParse

                            NoRule ->
                                Ok ()
                    )

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


canApplyRuleInTypeSystem : TypeSystem -> Rule -> Result String ()
canApplyRuleInTypeSystem typeSystem rule =
    if List.member rule <| rulesForTypeSystem typeSystem then
        Ok ()

    else
        Err "Rule is not in available for selected type system"


canApplyRuleToExpr : { rule : Rule, ctx : Context, term : Term, ty : Ty } -> Result String ()
canApplyRuleToExpr { rule, term, ty, ctx } =
    case rule of
        TVar ->
            case term of
                TmVar _ _ _ ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TVarInst ->
            case term of
                TmVar _ _ _ ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TIf ->
            case term of
                TmIf _ _ _ _ ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TTrue ->
            case term of
                TmConst _ TmTrue ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TFalse ->
            case term of
                TmConst _ TmFalse ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TAbs ->
            case term of
                TmAbs _ _ _ _ ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TApp ->
            case term of
                TmApp _ _ _ ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TTAbs ->
            case term of
                TmTAbs _ _ _ ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TTApp ->
            case term of
                TmTApp _ _ _ ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TLet ->
            case term of
                TmLet _ _ _ _ ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TLetGen ->
            case term of
                TmLet _ _ _ _ ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TGen ->
            case ty of
                TyAll _ _ ->
                    Ok ()

                _ ->
                    Err Lambda.RuleError.wrongRuleTerm

        TInst ->
            Ok ()

        NoRule ->
            Ok ()
