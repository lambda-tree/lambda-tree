module RuleTree.Update exposing (..)

import Lambda.Expression exposing (TypeSystem)
import Lambda.ExpressionUtils exposing (SubstitutionFtv, ftvCtx, ftvTerm, ftvTy, substFtvCtx, substFtvTerm, substFtvTy)
import Lambda.Inferer exposing (InferredTree, inferTree)
import Lambda.Parse exposing (parseCtx, parseTerm, parseType)
import Lambda.ParseTransform exposing (fromParseContext, fromParseTerm, fromParseType)
import Lambda.Rule exposing (ExprTree, ExprTreeContent, Rule(..))
import Lambda.Show.Print
import Lambda.Show.Text
import List.Extra
import RuleTree.Message exposing (..)
import RuleTree.Model exposing (..)
import RuleTree.ViewModel exposing (getExprTree)
import Set exposing (Set)
import Settings.Model
import Settings.Utils exposing (getTypeSystem)
import Substitutor.Model
import Substitutor.Utils exposing (parsedType, parsedVar)
import Utils.Outcome as Outcome
import Utils.Tree exposing (Tree(..), mapContentAtPath)


update : Msg -> Settings.Model.Model -> RuleTree -> RuleTree
update msg settings tree =
    case msg of
        TextChangedMsg path kind text ->
            updateTextAtPath path kind text tree

        HintTreeOneLevel path ->
            doHint1LevelAtPath (getTypeSystem settings) path tree

        RemoveMsg path ->
            setRule path NoRule tree

        RuleSelectedMsg path rule ->
            setRule path rule tree

        ClearTreeMsg ->
            emptyTree

        SelectRuleMsg _ ->
            Debug.todo "Update: SelectRuleMsg"

        RuleDropdownMsg path state ->
            mapContentAtPath path (\c -> { c | dropdown = state }) tree

        RuleStatusPopoverMsg path state ->
            mapContentAtPath path (\c -> { c | statusPopover = state }) tree

        HintTreeMsg path ->
            doHintAtPath (getTypeSystem settings) path tree

        HintRuleSelectionMsg path ->
            hintRuleSelection (getTypeSystem settings) path tree


hintRuleSelection : TypeSystem -> List Int -> RuleTree -> RuleTree
hintRuleSelection =
    hintWithMapperAtPath
        (\ruleTree (Node { rule } _) -> setRule [] rule ruleTree)


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


doHintAtPath : TypeSystem -> List Int -> RuleTree -> RuleTree
doHintAtPath =
    hintWithMapperAtPath (\_ -> inferredTreeToRuleTree)


doHint1LevelAtPath : TypeSystem -> List Int -> RuleTree -> RuleTree
doHint1LevelAtPath =
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


hintWithMapperAtPath : (RuleTree -> InferredTree -> RuleTree) -> TypeSystem -> List Int -> RuleTree -> RuleTree
hintWithMapperAtPath mapper typeSystem rootPath rootTree =
    let
        exprTree =
            getExprTree typeSystem rootTree

        walkTree : Set String -> List Int -> ExprTree -> Result String InferredTree
        walkTree ftvs path (Node ({ ctx, term, ty } as content) children) =
            case path of
                [] ->
                    case ( ctx, term ) of
                        ( Ok justCtx, Ok justTerm ) ->
                            inferTree typeSystem ftvs (ty |> Result.toMaybe) justCtx justTerm
                                |> Outcome.toResult
                                |> Result.mapError (Debug.log "doHintAtPath: inferTree error:")

                        _ ->
                            Err <| "There is an error in context or term of the expression to be inferred"

                x :: xs ->
                    let
                        otherSubTrees =
                            List.Extra.removeAt x children

                        subTreesFtvs =
                            otherSubTrees
                                |> List.map collectFtvsForExprTree
                                |> List.foldl Set.union Set.empty

                        subTreesAndMyFtvs =
                            subTreesFtvs |> Set.union (collectFtvsForExprTreeContent content)
                    in
                    List.Extra.getAt x children
                        |> Result.fromMaybe ("No child at index " ++ String.fromInt x)
                        |> Result.andThen (walkTree subTreesAndMyFtvs xs)

        maybeInferredTree =
            walkTree Set.empty rootPath exprTree
    in
    maybeInferredTree
        |> Result.map
            (\((Node { ss } _) as inferredTree) ->
                rootTree
                    |> Utils.Tree.mapTreeAtPath rootPath (\ruleTreeAtPath -> mapper ruleTreeAtPath inferredTree)
                    |> substFtvRuleTree ss
            )
        |> Result.withDefault rootTree


doHint : TypeSystem -> RuleTree -> RuleTree
doHint typeSystem ((Node ({ ctx, term, ty } as content) _) as t1) =
    let
        maybeCtx =
            parseCtx ctx
                |> Result.toMaybe
                |> Maybe.andThen (fromParseContext >> Result.toMaybe)

        maybeTerm =
            maybeCtx
                |> Maybe.andThen
                    (\justCtx ->
                        parseTerm term
                            |> Result.toMaybe
                            |> Maybe.andThen (fromParseTerm justCtx >> Result.toMaybe)
                    )

        maybeTy =
            maybeCtx
                |> Maybe.andThen
                    (\justCtx ->
                        parseType ty
                            |> Result.toMaybe
                            |> Maybe.map (fromParseType justCtx)
                    )
    in
    case ( maybeCtx, maybeTerm ) of
        ( Just justCtx, Just justTerm ) ->
            inferTree typeSystem Set.empty maybeTy justCtx justTerm
                |> Outcome.toResult
                |> Result.mapError (Debug.log "doHint: buildTree error:")
                |> Result.toMaybe
                |> Maybe.map
                    (Utils.Tree.map
                        (\inferredContent ->
                            { emptyTreeContent
                                | ctx = inferredContent.ctx |> Lambda.Show.Print.showCtx |> Lambda.Show.Text.show
                                , term = inferredContent.term |> Lambda.Show.Print.showTerm inferredContent.ctx |> Lambda.Show.Text.show
                                , ty = inferredContent.ty |> Lambda.Show.Print.showType inferredContent.ctx |> Lambda.Show.Text.show
                                , rule = inferredContent.rule
                            }
                        )
                    )
                |> Maybe.withDefault t1

        _ ->
            t1


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
setRule path rule tree =
    case tree of
        Node content children ->
            case path of
                [] ->
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

                idx :: subPath ->
                    Node content (children |> List.Extra.updateAt idx (setRule subPath rule))


updateTextAtPath : List Int -> TextKind -> String -> RuleTree -> RuleTree
updateTextAtPath path kind text tree =
    let
        preprocessed =
            Lambda.Parse.preprocess text
    in
    case tree of
        Node content children ->
            case path of
                [] ->
                    case kind of
                        CtxKind ->
                            Node { content | ctx = preprocessed } children

                        TermKind ->
                            Node { content | term = preprocessed } children

                        TyKind ->
                            Node { content | ty = preprocessed } children

                idx :: subPath ->
                    let
                        updatedChildren =
                            List.indexedMap
                                (\i t ->
                                    if i == idx then
                                        updateTextAtPath subPath kind text t

                                    else
                                        t
                                )
                                children
                    in
                    Node content updatedChildren
