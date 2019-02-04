module Lambda.Rule exposing (..)

import Either
import Lambda.Expression exposing (..)
import Lambda.ParseTransform exposing (fromParseContext, fromParseTerm, fromParseType)
import Maybe exposing (..)
import Lambda.Context exposing (..)
import Lambda.ContextUtils exposing (..)
import Model exposing (Tree(..), TreeModel)
import Lambda.Parse as P
import Parser exposing (deadEndsToString)
import Result


type TyRule
    = TVar
        { -- bottom
          bottomCtx : Context
        , -- term
          bottomTerm : Term
        , --
          bottomTy : Ty
        , -- top
          -- ctx
          topCtx : Context
        , -- t
          topTerm : Term
        , -- ty
          topTy : Ty
        }


type alias TypeStatement =
    { ctx : Context, term : Term, ty : Ty }


type alias ContainmentStatement =
    { ctx : Context, variable : Term }



--
--  RuleIf (If t1.. t2.. t3..)
--  rule1 rule2 rule3
--
--  RuleIf term typeT
--      rule1 rule2 rule3
--
--
--
--  RuleIf (RuleIfBottom t ty) (RuleIfTop rule1 rule2 rule3)
--
--  RuleIf (RuleIfBottom (If (\\0) (\\1) (\\0)) ty) (RuleIfTop rule1 rule2 rule3)
--      if rule1.ty === Bool && rule2.ty === ty && rule3.ty === ty
--
--
--
--  Rule ruleType RuleBottom RuleTop
--  Rule RuleIf
--  RuleBottom = RuleBottom term type
--  RuleTop = [Rule]
--
--
--  checkRule ctx rule =
--      case rule of
--          (TermIf t1 t2 t3) ty


checkRule : TyRule -> Bool
checkRule rule =
    case rule of
        TVar { bottomCtx, bottomTerm, bottomTy, topCtx, topTerm, topTy } ->
            bottomCtx
                == Debug.log "topCtx" topCtx
                && Debug.log "bottomTerm" bottomTerm
                == Debug.log "topTerm" topTerm
                && Debug.log "bottomTy" bottomTy
                == Debug.log "topTy" topTy
                && (case topTerm of
                        TmVar _ x _ ->
                            Debug.log "getbinding topCtx x::" (getbinding topCtx x) == Debug.log "Just VarBind::" (Just (VarBind bottomTy))

                        _ ->
                            False
                   )


getCtxTermTy : String -> String -> String -> Result String ( Context, Term, Ty )
getCtxTermTy ctx term ty =
    P.parseCtx ctx
        |> Result.andThen
            (\ctx1 ->
                P.parseTerm term
                    |> Result.andThen
                        (\term1 ->
                            P.parseType ty
                                |> Result.andThen
                                    (\ty1 ->
                                        Result.Ok
                                            ( fromParseContext ctx1
                                            , fromParseContext ctx1 |> Either.andThen (\parsedCtx -> fromParseTerm parsedCtx term1)
                                            , fromParseContext ctx1 |> Either.andThen (\parsedCtx -> fromParseType parsedCtx ty1)
                                            )
                                    )
                        )
            )
        |> Result.map (\( x, y, z ) -> Either.map3 (\a b c -> ( a, b, c )) x y z)
        |> (\r ->
                case r of
                    Result.Ok (Either.Left smt) ->
                        Result.Err "Parse Error"

                    Result.Ok (Either.Right smt) ->
                        Result.Ok smt

                    Result.Err smt ->
                        Result.Err "Parse Result Error"
           )


tryRule : TreeModel -> String
tryRule t =
    case t of
        Node { ctx, term, ty, rule } [ child ] ->
            case rule of
                Model.TVar ->
                    case child of
                        Node childC _ ->
                            let
                                bottom =
                                    getCtxTermTy ctx term ty

                                top =
                                    getCtxTermTy childC.ctx childC.term childC.ty
                            in
                                bottom
                                    |> Result.andThen
                                        (\( c1, tm1, ty1 ) ->
                                            top
                                                |> Result.andThen
                                                    (\( c2, tm2, ty2 ) ->
                                                        Result.Ok <|
                                                            checkRule
                                                                (TVar
                                                                    { bottomCtx = c1
                                                                    , bottomTerm = tm1
                                                                    , bottomTy = ty1
                                                                    , topCtx = c2
                                                                    , topTerm = tm2
                                                                    , topTy = ty2
                                                                    }
                                                                )
                                                    )
                                        )
                                    |> \r ->
                                        case r of
                                            Err text ->
                                                text

                                            Ok checks ->
                                                if checks then
                                                    "OOOOOKKKKK"
                                                else
                                                    "Nooooo!"

                _ ->
                    "Blah"

        _ ->
            "Doesn't have 1 child"
