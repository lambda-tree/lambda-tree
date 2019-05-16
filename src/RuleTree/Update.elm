module RuleTree.Update exposing (..)

import Lambda.Expression exposing (TypeSystem)
import Lambda.ExpressionUtils exposing (SubstitutionFtv, ftvCtx, ftvTerm, ftvTy, substFtvCtx, substFtvTerm, substFtvTy)
import Lambda.Inferer exposing (InferredTree, inferTree)
import Lambda.Parse exposing (parseCtx, parseTerm, parseType)
import Lambda.ParseTransform exposing (fromParseContext, fromParseTerm, fromParseType)
import Lambda.Rule exposing (ExprTree, ExprTreeContent, Rule(..))
import Lambda.Show.Print
import Lambda.Show.Text
import RuleTree.Message exposing (..)
import RuleTree.Model exposing (..)
import RuleTree.ViewModel exposing (getExprTree)
import Set exposing (Set)
import Settings.Model
import Settings.Utils exposing (getTypeSystem)
import Substitutor.Model
import Substitutor.Utils exposing (parsedType, parsedVar)
import Utils.Outcome as Outcome exposing (Outcome)
import Utils.Tree exposing (Tree(..), mapContentAtPath)


update : Msg -> Settings.Model.Model -> RuleTree -> Outcome String RuleTree
update msg settings tree =
    case msg of
        TextChangedMsg path kind text ->
            updateTextAtPath path kind text tree
                |> Outcome.fine

        HintPremisesMsg path ->
            hintPremises (getTypeSystem settings) path tree
                |> Outcome.fine

        RemoveMsg path ->
            setRule path NoRule tree
                |> Outcome.fine

        RuleSelectedMsg path rule ->
            setRule path rule tree
                |> Outcome.fine

        ClearTreeMsg ->
            emptyTree
                |> Outcome.fine

        RuleDropdownMsg path state ->
            mapContentAtPath path (\c -> { c | dropdown = state }) tree
                |> Outcome.fine

        RuleStatusPopoverMsg path state ->
            mapContentAtPath path (\c -> { c | statusPopover = state }) tree
                |> Outcome.fine

        HintBranchMsg path ->
            hintBranch (getTypeSystem settings) path tree

        HintRuleSelectionMsg path ->
            hintRuleSelection (getTypeSystem settings) path tree
                |> Outcome.fine


hintRuleSelection : TypeSystem -> List Int -> RuleTree -> RuleTree
hintRuleSelection typeSystem path tree =
    hintWithMapperAtPath (\ruleTree (Node { rule } _) -> setRule [] rule ruleTree) False typeSystem path tree
        |> Outcome.value


collectFtvsForExprTree : ExprTree -> Set String
collectFtvsForExprTree =
    Utils.Tree.foldr (collectFtvsForExprTreeContent >> Set.union) Set.empty


collectFtvsForExprTreeContent : ExprTreeContent -> Set String
collectFtvsForExprTreeContent { ctx, term, ty } =
    Set.empty
        |> Set.union
            (ctx
                |> Result.map ftvCtx
                |> Result.withDefault Set.empty
            )
        |> Set.union
            (term
                |> Result.map ftvTerm
                |> Result.withDefault Set.empty
            )
        |> Set.union
            (ty
                |> Result.map ftvTy
                |> Result.withDefault Set.empty
            )


inferredTreeToRuleTree : InferredTree -> RuleTree
inferredTreeToRuleTree =
    Utils.Tree.map
        (\inferredContent ->
            { emptyTreeContent
                | ctx = inferredContent.ctx |> Lambda.Show.Print.showCtx |> Lambda.Show.Text.show
                , term = inferredContent.term |> Lambda.Show.Print.showTerm inferredContent.ctx |> Lambda.Show.Text.show
                , ty = inferredContent.ty |> Lambda.Show.Print.showType inferredContent.ctx |> Lambda.Show.Text.show
                , rule = inferredContent.rule
            }
        )


hintBranch : TypeSystem -> List Int -> RuleTree -> Outcome String RuleTree
hintBranch =
    hintWithMapperAtPath (\_ -> inferredTreeToRuleTree) True


hintPremises : TypeSystem -> List Int -> RuleTree -> RuleTree
hintPremises typeSystem path tree =
    hintWithMapperAtPath
        (\_ inferredTree ->
            let
                cutTree =
                    Utils.Tree.take 2 inferredTree
                        |> Maybe.withDefault inferredTree
            in
            inferredTreeToRuleTree cutTree
                -- Remove the rules of the children
                |> (\(Node c children) -> Node c (List.map (setRule [] NoRule) children))
        )
        True
        typeSystem
        path
        tree
        |> Outcome.value


hintWithMapperAtPath : (RuleTree -> InferredTree -> RuleTree) -> Bool -> TypeSystem -> List Int -> RuleTree -> Outcome String RuleTree
hintWithMapperAtPath mapper applySubst typeSystem path tree =
    let
        exprTree =
            getExprTree typeSystem tree

        ftvs =
            exprTree
                |> Utils.Tree.mapTreeAtPath path (\(Node c _) -> Node c [])
                |> collectFtvsForExprTree

        maybeInferredTree =
            exprTree
                |> Utils.Tree.treeAtPath path
                |> Result.fromMaybe "Inconsistent state, no tree at path"
                |> Result.andThen
                    (\(Node { ctx, term, ty } _) ->
                        case ( ctx, term ) of
                            ( Ok justCtx, Ok justTerm ) ->
                                Ok <| inferTree typeSystem ftvs (ty |> Result.toMaybe) justCtx justTerm

                            _ ->
                                Err "There is an error in context or term of the expression to be inferred"
                    )
    in
    maybeInferredTree
        |> (Result.map <|
                Outcome.map
                    (\((Node { ss } _) as inferredTree) ->
                        tree
                            |> Utils.Tree.mapTreeAtPath path (\ruleTreeAtPath -> mapper ruleTreeAtPath inferredTree)
                            |> (if applySubst then
                                    substFtvRuleTree ss

                                else
                                    identity
                               )
                    )
           )
        |> Outcome.fromResult (Outcome.fine tree)
        |> Outcome.join


substFtvRuleTree : SubstitutionFtv -> RuleTree -> RuleTree
substFtvRuleTree ss tree =
    tree
        |> Utils.Tree.map
            (\({ ctx, term, ty } as o) ->
                let
                    maybeCtx =
                        parseCtx ctx
                            |> Result.toMaybe
                            |> Maybe.andThen (fromParseContext >> Result.toMaybe)
                in
                { o
                    | ctx =
                        maybeCtx
                            |> Maybe.andThen
                                (\c ->
                                    let
                                        substituted =
                                            substFtvCtx ss c
                                    in
                                    if substituted == c then
                                        Nothing

                                    else
                                        Just substituted
                                )
                            |> Maybe.map (Lambda.Show.Print.showCtx >> Lambda.Show.Text.show)
                            |> Maybe.withDefault ctx
                    , term =
                        maybeCtx
                            |> Maybe.andThen
                                (\justCtx ->
                                    parseTerm term
                                        |> Result.toMaybe
                                        |> Maybe.andThen (fromParseTerm justCtx >> Result.toMaybe)
                                        |> Maybe.andThen
                                            (\justTerm ->
                                                let
                                                    substituted =
                                                        substFtvTerm ss justTerm
                                                in
                                                if substituted == justTerm then
                                                    Nothing

                                                else
                                                    Just substituted
                                            )
                                        -- Shouldn't use the substituted CTX??
                                        |> Maybe.map (Lambda.Show.Print.showTerm justCtx >> Lambda.Show.Text.show)
                                )
                            |> Maybe.withDefault term
                    , ty =
                        maybeCtx
                            |> Maybe.andThen
                                (\justCtx ->
                                    parseType ty
                                        |> Result.toMaybe
                                        |> Maybe.map (fromParseType justCtx)
                                        |> Maybe.andThen
                                            (\justTy ->
                                                let
                                                    substituted =
                                                        substFtvTy ss justTy
                                                in
                                                if substituted == justTy then
                                                    Nothing

                                                else
                                                    Just substituted
                                            )
                                        |> Maybe.map (Lambda.Show.Print.showType justCtx >> Lambda.Show.Text.show)
                                )
                            |> Maybe.withDefault ty
                }
            )


doSubstitution : Substitutor.Model.Model -> RuleTree -> RuleTree
doSubstitution sm tree =
    case ( parsedType sm, parsedVar sm ) of
        ( Ok tyS, Ok varS ) ->
            let
                ss =
                    [ ( tyS, varS ) ]
            in
            substFtvRuleTree ss tree

        _ ->
            tree


setRule : List Int -> Rule -> RuleTree -> RuleTree
setRule path rule =
    Utils.Tree.mapTreeAtPath path
        (\(Node content _) ->
            case rule of
                TTrue ->
                    Node { content | rule = rule } []

                TFalse ->
                    Node { content | rule = rule } []

                TVar ->
                    Node { content | rule = rule } []

                TVarInst ->
                    Node { content | rule = rule } []

                TAbs ->
                    Node { content | rule = rule } [ emptyTree ]

                TApp ->
                    Node { content | rule = rule } [ emptyTree, emptyTree ]

                TIf ->
                    Node { content | rule = rule } [ emptyTree, emptyTree, emptyTree ]

                TTApp ->
                    Node { content | rule = rule } [ emptyTree ]

                TTAbs ->
                    Node { content | rule = rule } [ emptyTree ]

                TLet ->
                    Node { content | rule = rule } [ emptyTree, emptyTree ]

                TLetGen ->
                    Node { content | rule = rule } [ emptyTree, emptyTree ]

                TGen ->
                    Node { content | rule = rule } [ emptyTree ]

                TInst ->
                    Node { content | rule = rule } [ emptyTree ]

                NoRule ->
                    Node { content | rule = rule } []
        )


updateTextAtPath : List Int -> TextKind -> String -> RuleTree -> RuleTree
updateTextAtPath path kind text =
    Utils.Tree.mapContentAtPath path
        (\content ->
            case kind of
                CtxKind ->
                    { content | ctx = text }

                TermKind ->
                    { content | term = text }

                TyKind ->
                    { content | ty = text }
        )
