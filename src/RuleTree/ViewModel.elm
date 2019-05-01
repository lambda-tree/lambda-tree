module RuleTree.ViewModel exposing (..)

import Bootstrap.Dropdown as Dropdown
import Bootstrap.Popover as Popover
import Lambda.Expression exposing (TypeSystem)
import Lambda.ExpressionUtils exposing (isCtxInTypeSystem, isTermInTypeSystem, isTyInTypeSystem)
import Lambda.Parse exposing (parseCtx, parseTerm, parseType)
import Lambda.ParseTransform exposing (fromParseContext, fromParseTerm, fromParseType)
import Lambda.Rule exposing (ExprError(..), ExprTree, Rule(..), TyRule, tryRule)
import RuleTree.Model exposing (RuleTree)
import Utils.Tree exposing (Tree(..))


type alias TreeViewDataError =
    { row : Int, col : Int }


type alias TreeViewDataResult =
    { text : String, error : Maybe ExprError }


type alias TreeViewData =
    Tree
        { ctx : TreeViewDataResult
        , term : TreeViewDataResult
        , ty : TreeViewDataResult
        , rule : Rule
        , result : Result String ()
        , dropdown : Dropdown.State
        , statusPopover : Popover.State
        }


getExprTree : TypeSystem -> RuleTree -> ExprTree
getExprTree typeSystem t =
    t
        |> Utils.Tree.map
            (\{ ctx, term, ty, rule } ->
                let
                    ctxExpr =
                        parseCtx ctx
                            |> Result.mapError ParseError
                            |> Result.andThen
                                (\parsedCtx ->
                                    fromParseContext parsedCtx
                                        |> Result.mapError ParseTransformError
                                )
                            |> Result.andThen
                                (\parsedCtx ->
                                    if isCtxInTypeSystem typeSystem parsedCtx then
                                        Ok parsedCtx

                                    else
                                        Err NotInTypeSystemError
                                )

                    termExpr =
                        parseTerm term
                            |> Result.mapError ParseError
                            |> Result.andThen
                                (\parsedTerm ->
                                    ctxExpr
                                        |> Result.mapError (\_ -> PrerequisiteDataError)
                                        |> Result.andThen
                                            (\okContext ->
                                                fromParseTerm okContext parsedTerm
                                                    |> Result.mapError ParseTransformError
                                            )
                                )
                            |> Result.andThen
                                (\parsedTerm ->
                                    if isTermInTypeSystem typeSystem parsedTerm then
                                        Ok parsedTerm

                                    else
                                        Err NotInTypeSystemError
                                )

                    tyExpr =
                        parseType ty
                            |> Result.mapError ParseError
                            |> Result.andThen
                                (\parsedTy ->
                                    ctxExpr
                                        |> Result.mapError (\_ -> PrerequisiteDataError)
                                        |> Result.map (\okContext -> fromParseType okContext parsedTy)
                                )
                            |> Result.andThen
                                (\parsedTy ->
                                    if isTyInTypeSystem typeSystem parsedTy then
                                        Ok parsedTy

                                    else
                                        Err NotInTypeSystemError
                                )
                in
                { ctx = ctxExpr
                , term = termExpr
                , ty = tyExpr
                , rule = rule
                }
            )


getTreeViewData : TypeSystem -> RuleTree -> TreeViewData
getTreeViewData typeSystem tree =
    let
        zipper origNode _ exprNode exprTree =
            let
                extractError result =
                    case result of
                        Err e ->
                            Just e

                        Ok _ ->
                            Nothing
            in
            { ctx = { text = origNode.ctx, error = extractError exprNode.ctx }
            , term = { text = origNode.term, error = extractError exprNode.term }
            , ty = { text = origNode.ty, error = extractError exprNode.ty }
            , rule = origNode.rule
            , result = tryRule typeSystem exprTree
            , dropdown = origNode.dropdown
            , statusPopover = origNode.statusPopover
            }
    in
    Utils.Tree.zipWithExtra zipper tree (getExprTree typeSystem tree)
