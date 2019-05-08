module Lambda.Inferer exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (addbinding, emptycontext, getbinding)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (..)
import Lambda.Rule exposing (Rule(..))
import Set exposing (Set)
import Utils.Tree exposing (Tree(..))


type alias InferredTreeContent =
    { ctx : Context, term : Term, ty : Ty, rule : Rule, ss : SubstitutionFtv, ftvs : Set String }


type alias InferredTree =
    Tree InferredTreeContent


expandGen : InferredTree -> InferredTree
expandGen ((Node content children) as tree) =
    case content.ty of
        TyAll _ (TyAll _ _) ->
            Node { content | rule = TGen } [ expandGen <| Node { content | ty = degeneralizeTypeTop content.ctx content.ty } children ]

        _ ->
            tree


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


{-| Optimize by running only on the final tree
-}
unifyWithRootType : TypeSystem -> Set String -> Maybe Ty -> InferredTree -> InferredTree
unifyWithRootType typeSystem ftvs maybeTy ((Node c children) as tree) =
    maybeTy
        |> Maybe.andThen
            (\ty ->
                case ( ty, typeSystem ) of
                    ( TyAll _ _, HM NonDeterministic ) ->
                        unifyType (Debug.log "c.ty" c.ty) (Debug.log "degenTy" (degeneralizeType emptycontext (Debug.log "ty" ty)))
                            |> Result.map
                                (\ss ->
                                    let
                                        ((Node n1 _) as bt1) =
                                            applySSTree (Node { c | ss = ss ++ c.ss } children)
                                    in
                                    expandGen <|
                                        Node
                                            { ctx = n1.ctx
                                            , term = n1.term
                                            , ty = gen ftvs n1.ctx n1.ty
                                            , ss = n1.ss
                                            , rule = TGen
                                            , ftvs = n1.ftvs
                                            }
                                            [ bt1 ]
                                )
                            |> Result.toMaybe

                    _ ->
                        (Debug.log "unif" <| unifyType (Debug.log "c.ty" c.ty) (Debug.log "ty" ty))
                            |> Result.map (\ss -> applySSTree (Node { c | ss = ss ++ c.ss } children))
                            |> Result.toMaybe
            )
        |> Maybe.withDefault tree


{-| Optimize by running only on the final tree
-}
unifyToRootCtxTerm : Context -> Term -> Maybe Ty -> InferredTree -> InferredTree
unifyToRootCtxTerm ctx term ty ((Node c children) as tree) =
    let
        _ =
            Debug.log "unifyToRootCtxTerm" ( c.ctx, ctx )
    in
    unifyTypeCtx c.ctx ctx
        |> Result.andThen
            (\s1 ->
                let
                    _ =
                        Debug.log "s11111" s1
                in
                unifyTypeTerm (substFtvTerm s1 c.term) (substFtvTerm s1 term)
                    |> Result.map (\s2 -> s2 ++ s1)
            )
        |> Result.map (\ss -> applySSTree (Node { c | ss = ss ++ c.ss } children))
        |> Result.withDefault tree


inferTree : TypeSystem -> Set String -> Maybe Ty -> Context -> Term -> Result String InferredTree
inferTree typeSystem rootFtvs rootType =
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
                                HM hmFlavor ->
                                    let
                                        instTy =
                                            inst (ftvs |> Set.union (topBoundVars ty)) ctx ty
                                    in
                                    case hmFlavor of
                                        SyntaxDirected ->
                                            Ok <|
                                                Node
                                                    { ctx = ctx
                                                    , term = t
                                                    , ty = instTy
                                                    , ss = []
                                                    , rule = TVarInst
                                                    , ftvs = ftvTy instTy
                                                    }
                                                    []

                                        NonDeterministic ->
                                            if Set.size (topBoundVars ty) > 0 then
                                                Ok <|
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
                                |> Maybe.withDefault (TyName <| freshVarName ftvs "X")

                        ctx1 =
                            addbinding ctx varName (VarBind fromType)
                    in
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
                                buildTree (ftvs |> Set.union n1.ftvs) (substFtvCtx n1.ss ctx) t2
                                    |> Result.andThen
                                        (\((Node n2 _) as bt2) ->
                                            case typeSystem of
                                                HM _ ->
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
                                                    unifyType (TyArr n2.ty tauPrime) (substFtvTy n2.ss n1.ty)
                                                        |> Result.map
                                                            (\s3 ->
                                                                Node
                                                                    { ctx = ctx
                                                                    , term = t
                                                                    , ty = substFtvTy s3 tauPrime
                                                                    , rule = TApp
                                                                    , ss = s3 ++ n2.ss ++ n1.ss
                                                                    , ftvs = n1.ftvs |> Set.union n2.ftvs |> Set.union (ftvTy tauPrime)
                                                                    }
                                                                    [ bt1, bt2 ]
                                                            )

                                                _ ->
                                                    case n1.ty of
                                                        TyArr _ tyTo ->
                                                            Ok <|
                                                                Node
                                                                    { ctx = ctx
                                                                    , term = t
                                                                    , ty = tyTo
                                                                    , rule = TApp
                                                                    , ss = n2.ss ++ n1.ss
                                                                    , ftvs = n1.ftvs |> Set.union n2.ftvs
                                                                    }
                                                                    [ bt1, bt2 ]

                                                        _ ->
                                                            -- Ty= Nothing
                                                            Err <| "Applying argument to type that is not an abstraction"
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
                                                gen ftvs ctx1 n1.ty
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
                                                                [ expandGen <|
                                                                    Node
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

                TmTAbs _ tyVarName _ ->
                    (if Set.member tyVarName ftvs then
                        Err <| "Type abstraction variable '" ++ tyVarName ++ "' is free"

                     else
                        Ok ()
                    )
                        |> Result.andThen
                            (\_ ->
                                degeneralizeTermTop ctx t
                                    |> buildTree (ftvs |> Set.insert tyVarName) ctx
                            )
                        |> Result.andThen
                            (\((Node n1 _) as bt1) ->
                                Ok <|
                                    Node
                                        { ctx = ctx
                                        , term = generalizeTermTop ctx n1.term tyVarName
                                        , ty = generalizeTypeTop ctx n1.ty tyVarName
                                        , rule = TTAbs
                                        , ss = n1.ss
                                        , ftvs = n1.ftvs |> Set.insert tyVarName
                                        }
                                        [ bt1 ]
                            )

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
    \rootCtx rootTerm ->
        let
            ftvs =
                rootFtvs
                    |> Set.union (ftvCtx rootCtx)
                    |> Set.union (ftvTerm rootTerm)
                    |> Set.union
                        (rootType
                            |> Maybe.map ftvTy
                            |> Maybe.withDefault Set.empty
                        )

            builtTree =
                buildTree
                    ftvs
                    rootCtx
                    rootTerm

            _ =
                builtTree |> Result.map (\(Node c _) -> Debug.log "builtTree SS: " c.ss)

            _ =
                builtTree |> Result.map (\(Node c _) -> Debug.log "builtTree ftvs: " c.ftvs)
        in
        builtTree
            |> Result.map applySSTree
            |> Result.map
                (case typeSystem of
                    HM _ ->
                        unifyToRootCtxTerm rootCtx rootTerm Nothing

                    _ ->
                        identity
                )
            |> Result.map (unifyWithRootType typeSystem ftvs rootType)


w : Context -> Term -> Result String ( SubstitutionFtv, Ty )
w ctx term =
    inferTree (HM SyntaxDirected) Set.empty Nothing ctx term
        |> Result.map (\(Node { ty, ss } _) -> ( ss, ty ))


typeOf : TypeSystem -> Context -> Term -> Result String Ty
typeOf typeSystem ctx term =
    inferTree typeSystem Set.empty Nothing ctx term
        |> Result.map (\(Node { ty } _) -> ty)
        |> Result.map (gen (ftvCtx ctx) ctx)
