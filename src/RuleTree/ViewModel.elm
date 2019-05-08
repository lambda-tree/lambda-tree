module RuleTree.ViewModel exposing (..)

import Bootstrap.Dropdown as Dropdown
import Bootstrap.Popover as Popover
import Lambda.Expression exposing (TypeSystem)
import Lambda.ExpressionUtils exposing (isCtxInTypeSystem, isTermInTypeSystem, isTyInTypeSystem)
import Lambda.Parse exposing (parseCtx, parseTerm, parseType)
import Lambda.ParseTransform exposing (fromParseContext, fromParseTerm, fromParseType)
import Lambda.Rule exposing (ExprError(..), ExprTree, ExprTreeContent, Rule(..), TyRule, tryRule)
import RuleTree.Model exposing (RuleTree, RuleTreeContent)
import Utils.Tree exposing (Tree(..))
import Utils.Utils exposing (extractError)


type alias TreeViewDataResult =
    { text : String, error : Maybe ExprError }


type alias TreeViewDataContent =
    { ctx : TreeViewDataResult
    , term : TreeViewDataResult
    , ty : TreeViewDataResult
    , rule : Rule
    , result : Result String ()
    , dropdown : Dropdown.State
    , statusPopover : Popover.State
    }


type alias TreeViewData =
    Tree TreeViewDataContent


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
                            |> Result.mapError
                                (\x ->
                                    if String.isEmpty ty then
                                        EmptyTypeError

                                    else
                                        ParseError x
                                )
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
        zipper : RuleTreeContent -> RuleTree -> ExprTreeContent -> ExprTree -> TreeViewDataContent
        zipper origNode _ exprNode exprTree =
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


isTreeEmpty : RuleTree -> Bool
isTreeEmpty (Node { ctx, term, ty, rule } children) =
    List.all String.isEmpty [ ctx, term, ty ]
        && (rule == NoRule)
        && List.isEmpty children
