module Inferer.Inferer exposing (..)

import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (addbinding, getbinding)
import Lambda.Expression exposing (..)
import Lambda.ExpressionUtils exposing (..)
import Model exposing (Rule(..))
import Set exposing (Set)
import Utils.Tree exposing (Tree(..))


type alias BuiltTree =
    Tree { ctx : Context, term : Term, ty : Ty, rule : Rule, ss : SubstitutionFtv }


buildTree : Context -> Term -> Result String BuiltTree
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
                    Ok <|
                        Node
                            { ctx = ctx
                            , term = t
                            , ty = inst ctx ty
                            , ss = []
                            , rule = TVarInst
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
                            , ty = substFtv n1.ss (TyArr fromType n1.ty)
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
                                    unifyType (substFtv n2.ss n1.ty) (TyArr n2.ty tauPrime)
                                        |> Result.map
                                            (\s3 ->
                                                Node
                                                    { ctx = ctx
                                                    , term = t
                                                    , ty = substFtv s3 tauPrime
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
                        let
                            ctx1 =
                                substFtvCtx n1.ss ctx

                            genTy =
                                gen ctx1 n1.ty
                        in
                        buildTree (addbinding ctx varName (VarBind genTy)) t2
                            |> Result.map
                                (\((Node n2 _) as bt2) ->
                                    Node
                                        { ctx = ctx
                                        , term = t
                                        , ty = n2.ty
                                        , rule = TLetGen
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
                        unifyType (substFtv n1.ss (TyName tyVarName)) (TyName tyVarName)
                            |> Result.map
                                (\s2 ->
                                    Node
                                        { ctx = ctx
                                        , term = t
                                        , ty = substFtv s2 n1.ty
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
