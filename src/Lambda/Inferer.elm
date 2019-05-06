module Lambda.Inferer exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (addbinding, getbinding)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (..)
import Lambda.Rule exposing (Rule(..))
import Set exposing (Set)
import Utils.Tree exposing (Tree(..))


type alias InferredTreeContent =
    { ctx : Context, term : Term, ty : Ty, rule : Rule, ss : SubstitutionFtv, ftvs : Set String }


type alias InferredTree =
    Tree InferredTreeContent


ftvTree : InferredTree -> Set String
ftvTree =
    Utils.Tree.foldr
        (\{ ctx, term, ty, ss } ->
            Set.union (ftvCtx ctx)
                >> Set.union (ftvTerm term)
                >> Set.union (ftvTy ty)
                >> Set.union
                    (List.foldl
                        (\( ssTy, ssVar ) ->
                            Set.union (ftvTy ssTy) >> Set.union (Set.singleton ssVar)
                        )
                        Set.empty
                        ss
                    )
        )
        Set.empty


{-| Optimize by running only on the final tree
-}
applySSTree : InferredTree -> InferredTree
applySSTree ((Node { ss } _) as tree) =
    tree
        |> Utils.Tree.map
            (\({ ctx, term, ty } as c) ->
                { c
                    | ctx = substFtvCtx ss ctx
                    , term = substFtvTerm ss term
                    , ty = substFtvTy ss ty
                }
            )


inferTree : TypeSystem -> Context -> Term -> Result String InferredTree
inferTree typeSystem =
    let
        buildTree : Set String -> Context -> Term -> Result String InferredTree
        buildTree ftvs ctx t =
            case t of
                TmConst _ c ->
                    case c of
                        TmTrue ->
                            Ok <|
                                Node
                                    { ctx = ctx
                                    , term = t
                                    , ty = TyConst TyBool
                                    , ss = []
                                    , rule = TTrue
                                    , ftvs = Set.empty
                                    }
                                    []

                        TmFalse ->
                            Ok <|
                                Node
                                    { ctx = ctx
                                    , term = t
                                    , ty = TyConst TyBool
                                    , ss = []
                                    , rule = TFalse
                                    , ftvs = Set.empty
                                    }
                                    []

                TmVar _ x _ ->
                    case getbinding ctx x of
                        Just (VarBind ty) ->
                            case typeSystem of
                                HM SyntaxDirected ->
                                    Ok <|
                                        let
                                            instTy =
                                                inst ftvs ctx ty
                                        in
                                        Node
                                            { ctx = ctx
                                            , term = t
                                            , ty = instTy
                                            , ss = []
                                            , rule = TVarInst
                                            , ftvs = ftvTy instTy
                                            }
                                            []

                                HM NonDeterministic ->
                                    if Set.size (topBoundVars ty) > 0 then
                                        Ok <|
                                            let
                                                instTy =
                                                    inst ftvs ctx ty
                                            in
                                            Node
                                                { ctx = ctx
                                                , term = t
                                                , ty = instTy
                                                , ss = []
                                                , rule = TInst
                                                , ftvs = ftvTy instTy
                                                }
                                                [ Node
                                                    { ctx = ctx
                                                    , term = t
                                                    , ty = ty
                                                    , ss = []
                                                    , rule = TVar
                                                    , ftvs = Set.empty
                                                    }
                                                    []
                                                ]

                                    else
                                        Ok <|
                                            Node
                                                { ctx = ctx
                                                , term = t
                                                , ty = ty
                                                , ss = []
                                                , rule = TVar
                                                , ftvs = Set.empty
                                                }
                                                []

                                _ ->
                                    Ok <|
                                        Node
                                            { ctx = ctx
                                            , term = t
                                            , ty = ty
                                            , ss = []
                                            , rule = TVar
                                            , ftvs = Set.empty
                                            }
                                            []

                        _ ->
                            Err "Var is not bound in the context with type"

                TmAbs _ varName maybeType t1 ->
                    let
                        fromType =
                            maybeType
                                |> Maybe.withDefault (TyName <| freshVarName (ftvCtx ctx |> Set.union ftvs) "X")

                        ctx1 =
                            addbinding ctx varName (VarBind fromType)
                    in
                    -- FTV Keep
                    buildTree (ftvs |> Set.union (ftvTy fromType)) ctx1 t1
                        |> Result.map
                            (\((Node n1 _) as bt1) ->
                                Node
                                    { ctx = ctx
                                    , term = t
                                    , ty = substFtvTy n1.ss (TyArr fromType n1.ty)
                                    , rule = TAbs
                                    , ss = n1.ss

                                    -- FTV Keep
                                    , ftvs =
                                        n1.ftvs
                                            |> Set.union
                                                (case maybeType of
                                                    Just _ ->
                                                        Set.empty

                                                    Nothing ->
                                                        ftvTy fromType
                                                )
                                    }
                                    [ bt1 ]
                            )

                TmApp _ t1 t2 ->
                    buildTree ftvs ctx t1
                        |> Result.andThen
                            (\((Node n1 _) as bt1) ->
                                -- !!! Shouldn't also `substFtvTerm` in t2 -> to substitute for the resolved  annotations
                                buildTree (ftvs |> Set.union n1.ftvs) (substFtvCtx n1.ss ctx) t2
                                    |> Result.andThen
                                        (\((Node n2 _) as bt2) ->
                                            let
                                                tauPrime =
                                                    TyName <|
                                                        freshVarName
                                                            (ftvCtx ctx
                                                                |> Set.union (ftvTy n1.ty)
                                                                |> Set.union (ftvTy n2.ty)
                                                                |> Set.union ftvs
                                                                |> Set.union (ftvTree bt1)
                                                                |> Set.union (ftvTree bt2)
                                                            )
                                                            "X"
                                            in
                                            unifyType (substFtvTy n2.ss n1.ty) (TyArr n2.ty tauPrime)
                                                |> Result.map
                                                    (\s3 ->
                                                        Node
                                                            { ctx = ctx
                                                            , term = t
                                                            , ty = substFtvTy s3 tauPrime
                                                            , rule = TApp
                                                            , ss = s3 ++ n2.ss ++ n1.ss

                                                            -- FTV Keep
                                                            , ftvs = n1.ftvs |> Set.union n2.ftvs |> Set.union (ftvTy tauPrime)
                                                            }
                                                            [ bt1, bt2 ]
                                                    )
                                        )
                            )

                TmLet _ varName t1 t2 ->
                    buildTree ftvs ctx t1
                        |> Result.andThen
                            (\((Node n1 _) as bt1) ->
                                case typeSystem of
                                    HM hmType ->
                                        let
                                            ctx1 =
                                                substFtvCtx n1.ss ctx

                                            genTy =
                                                gen (ftvs |> Set.union n1.ftvs) ctx1 n1.ty
                                        in
                                        buildTree (ftvs |> Set.union n1.ftvs) (addbinding ctx varName (VarBind genTy)) t2
                                            |> Result.map
                                                (\((Node n2 _) as bt2) ->
                                                    case hmType of
                                                        SyntaxDirected ->
                                                            Node
                                                                { ctx = ctx
                                                                , term = t
                                                                , ty = n2.ty
                                                                , rule = TLetGen
                                                                , ss = n2.ss ++ n1.ss
                                                                , ftvs = n1.ftvs |> Set.union n2.ftvs
                                                                }
                                                                [ bt1, bt2 ]

                                                        NonDeterministic ->
                                                            Node
                                                                { ctx = ctx
                                                                , term = t
                                                                , ty = n2.ty
                                                                , rule = TLet
                                                                , ss = n2.ss ++ n1.ss
                                                                , ftvs = n1.ftvs |> Set.union n2.ftvs
                                                                }
                                                                [ Node
                                                                    { ctx = n1.ctx
                                                                    , term = n1.term
                                                                    , ty = genTy
                                                                    , rule = TGen
                                                                    , ss = n1.ss
                                                                    , ftvs = n1.ftvs
                                                                    }
                                                                    [ bt1 ]
                                                                , bt2
                                                                ]
                                                )

                                    _ ->
                                        buildTree ftvs (addbinding ctx varName (VarBind n1.ty)) t2
                                            |> Result.map
                                                (\((Node n2 _) as bt2) ->
                                                    Node
                                                        { ctx = ctx
                                                        , term = t
                                                        , ty = n2.ty
                                                        , rule = TLet
                                                        , ss = n2.ss ++ n1.ss
                                                        , ftvs = n1.ftvs |> Set.union n2.ftvs
                                                        }
                                                        [ bt1, bt2 ]
                                                )
                            )

                -- Extension of W to work with System F terms
                TmTAbs _ tyVarName _ ->
                    degeneralizeTermTop ctx t
                        |> buildTree (ftvs |> Set.insert tyVarName) ctx
                        |> Result.andThen
                            (\((Node n1 _) as bt1) ->
                                -- If var is supposed to be some more special type, then fail
                                -- Move this to unify type case of TyAll
                                -- what if there is some constraint on the type variable?? -> should have been explicitly specified in types of expressions!!!!
                                unifyType (substFtvTy n1.ss (TyName tyVarName)) (TyName tyVarName)
                                    |> Result.andThen
                                        (\s2 ->
                                            case s2 of
                                                [ ( TyName _, _ ) ] ->
                                                    Ok <|
                                                        Node
                                                            { ctx = ctx
                                                            , term = t
                                                            , ty = substFtvTy s2 n1.ty
                                                            , rule = TTAbs
                                                            , ss = s2 ++ n1.ss
                                                            , ftvs = n1.ftvs |> Set.insert tyVarName
                                                            }
                                                            [ bt1 ]

                                                _ ->
                                                    Err <| "Type variable '" ++ tyVarName ++ "' is used in a more constrained way"
                                        )
                            )
                        |> Result.map (\(Node n1 c1) -> Node { n1 | ty = generalizeTypeTop ctx n1.ty tyVarName } c1)

                TmTApp _ t1 tyS ->
                    buildTree ftvs ctx t1
                        |> Result.andThen
                            (\((Node n1 _) as bt1) ->
                                case n1.ty of
                                    TyAll _ ty1 ->
                                        Ok <|
                                            Node
                                                { ctx = ctx
                                                , term = t

                                                -- Keep it this way, or use substitution of ftv?
                                                , ty = typeSubstTop tyS ty1
                                                , rule = TTApp
                                                , ss = n1.ss
                                                , ftvs = n1.ftvs
                                                }
                                                [ bt1 ]

                                    _ ->
                                        Err "Type can be applied only on type abstraction term"
                            )

                _ ->
                    Err "Not implemented"
    in
    \x y ->
        let
            builtTree =
                buildTree (ftvCtx x |> Set.union (ftvTerm y)) x y

            _ =
                builtTree |> Result.map (\(Node c _) -> Debug.log "builtTree SS: " c.ss)

            _ =
                builtTree |> Result.map (\(Node c _) -> Debug.log "builtTree ftvs: " c.ftvs)
        in
        builtTree
            |> Result.map applySSTree


w : Context -> Term -> Result String ( SubstitutionFtv, Ty )
w ctx term =
    inferTree (HM SyntaxDirected) ctx term
        |> Result.map (\(Node { ty, ss } _) -> ( ss, ty ))


typeOf : TypeSystem -> Context -> Term -> Result String Ty
typeOf typeSystem ctx term =
    inferTree typeSystem ctx term
        |> Result.map (\(Node { ty } _) -> ty)
        |> Result.map (gen (ftvCtx ctx) ctx)
