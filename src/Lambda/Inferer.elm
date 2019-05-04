module Lambda.Inferer exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (addbinding, getbinding)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (..)
import Lambda.Rule exposing (Rule(..))
import Set exposing (Set)
import Utils.Tree exposing (Tree(..))


type alias InferredTreeContent =
    { ctx : Context, term : Term, ty : Ty, rule : Rule, ss : SubstitutionFtv }


type alias InferredTree =
    Tree InferredTreeContent


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
        buildTree ctx t =
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
                                    }
                                    []

                TmVar _ x _ ->
                    case getbinding ctx x of
                        Just (VarBind ty) ->
                            case typeSystem of
                                HM SyntaxDirected ->
                                    Ok <|
                                        Node
                                            { ctx = ctx
                                            , term = t
                                            , ty = inst ctx ty
                                            , ss = []
                                            , rule = TVarInst
                                            }
                                            []

                                HM NonDeterministic ->
                                    if Set.size (topBoundVars ty) > 0 then
                                        Ok <|
                                            Node
                                                { ctx = ctx
                                                , term = t
                                                , ty = inst ctx ty
                                                , ss = []
                                                , rule = TInst
                                                }
                                                [ Node
                                                    { ctx = ctx
                                                    , term = t
                                                    , ty = ty
                                                    , ss = []
                                                    , rule = TVar
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
                                            }
                                            []

                        _ ->
                            Err "Var is not bound in the context with type"

                TmAbs _ varName maybeType t1 ->
                    let
                        fromType =
                            maybeType
                                |> Maybe.withDefault (TyName <| freshVarName (ftvCtx ctx) "X")

                        ctx1 =
                            addbinding ctx varName (VarBind fromType)
                    in
                    buildTree ctx1 t1
                        |> Result.map
                            (\((Node n1 _) as bt1) ->
                                Node
                                    { ctx = ctx
                                    , term = t
                                    , ty = substFtvTy n1.ss (TyArr fromType n1.ty)
                                    , rule = TAbs
                                    , ss = n1.ss
                                    }
                                    [ bt1 ]
                            )

                TmApp _ t1 t2 ->
                    buildTree ctx t1
                        |> Result.andThen
                            (\((Node n1 _) as bt1) ->
                                buildTree (substFtvCtx n1.ss ctx) t2
                                    |> Result.andThen
                                        (\((Node n2 _) as bt2) ->
                                            let
                                                tauPrime =
                                                    TyName <|
                                                        freshVarName
                                                            (ftvCtx ctx
                                                                |> Set.union (ftvTy n1.ty)
                                                                |> Set.union (ftvTy n2.ty)
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
                                                            }
                                                            [ bt1, bt2 ]
                                                    )
                                        )
                            )

                TmLet _ varName t1 t2 ->
                    buildTree ctx t1
                        |> Result.andThen
                            (\((Node n1 _) as bt1) ->
                                case typeSystem of
                                    HM hmType ->
                                        let
                                            ctx1 =
                                                substFtvCtx n1.ss ctx

                                            genTy =
                                                gen ctx1 n1.ty
                                        in
                                        buildTree (addbinding ctx varName (VarBind genTy)) t2
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
                                                                }
                                                                [ bt1, bt2 ]

                                                        NonDeterministic ->
                                                            Node
                                                                { ctx = ctx
                                                                , term = t
                                                                , ty = n2.ty
                                                                , rule = TLet
                                                                , ss = n2.ss ++ n1.ss
                                                                }
                                                                [ Node
                                                                    { ctx = n1.ctx
                                                                    , term = n1.term
                                                                    , ty = genTy
                                                                    , rule = TGen
                                                                    , ss = n1.ss
                                                                    }
                                                                    [ bt1 ]
                                                                , bt2
                                                                ]
                                                )

                                    _ ->
                                        buildTree (addbinding ctx varName (VarBind n1.ty)) t2
                                            |> Result.map
                                                (\((Node n2 _) as bt2) ->
                                                    Node
                                                        { ctx = ctx
                                                        , term = t
                                                        , ty = n2.ty
                                                        , rule = TLet
                                                        , ss = n2.ss ++ n1.ss
                                                        }
                                                        [ bt1, bt2 ]
                                                )
                            )

                -- Extension of W to work with System F terms
                TmTAbs _ tyVarName _ ->
                    degeneralizeTermTop ctx t
                        |> buildTree ctx
                        |> Result.andThen
                            (\((Node n1 _) as bt1) ->
                                unifyType (substFtvTy n1.ss (TyName tyVarName)) (TyName tyVarName)
                                    |> Result.map
                                        (\s2 ->
                                            Node
                                                { ctx = ctx
                                                , term = t
                                                , ty = substFtvTy s2 n1.ty
                                                , rule = TTAbs
                                                , ss = s2 ++ n1.ss
                                                }
                                                [ bt1 ]
                                        )
                            )
                        |> Result.map (\(Node n1 c1) -> Node { n1 | ty = generalizeTypeTop ctx n1.ty tyVarName } c1)

                TmTApp _ t1 tyS ->
                    buildTree ctx t1
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
                                                }
                                                [ bt1 ]

                                    _ ->
                                        Err "Type can be applied only on type abstraction term"
                            )

                _ ->
                    Err "Not implemented"
    in
    \x y ->
        buildTree x y
            |> Result.map applySSTree