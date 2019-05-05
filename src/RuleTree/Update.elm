module RuleTree.Update exposing (..)

import Lambda.Expression exposing (TypeSystem)
import Lambda.ExpressionUtils exposing (substFtvCtx, substFtvTerm, substFtvTy)
import Lambda.Inferer exposing (inferTree)
import Lambda.Parse exposing (parseCtx, parseTerm, parseType)
import Lambda.ParseTransform exposing (fromParseContext, fromParseTerm, fromParseType)
import Lambda.Rule exposing (Rule(..))
import Lambda.Show.Print
import Lambda.Show.Text
import List.Extra
import RuleTree.Message exposing (..)
import RuleTree.Model exposing (..)
import Settings.Model
import Settings.Utils exposing (getTypeSystem)
import Substitutor.Model
import Substitutor.Utils exposing (parsedType, parsedVar)
import Utils.Tree exposing (Tree(..), mapContentAtPath)


update : Msg -> Settings.Model.Model -> RuleTree -> RuleTree
update msg settings tree =
    case msg of
        TextChangedMsg path kind text ->
            updateTextAtPath path kind text tree

        HintMsg ->
            doHint (getTypeSystem settings) tree

        RemoveMsg path ->
            setRule path NoRule tree

        RuleSelectedMsg path rule ->
            setRule path rule tree

        RuleClickedMsg _ ->
            Debug.todo "Update: RuleClickedMsg"

        SelectRuleMsg _ ->
            Debug.todo "Update: SelectRuleMsg"

        RuleDropdownMsg path state ->
            mapContentAtPath path (\c -> { c | dropdown = state }) tree

        RuleStatusPopoverMsg path state ->
            mapContentAtPath path (\c -> { c | statusPopover = state }) tree

        HintTree path ->
            doHint (getTypeSystem settings) tree

        HintRuleSelection path ->
            hintRuleSelection (getTypeSystem settings) path tree


hintRuleSelection : TypeSystem -> List Int -> RuleTree -> RuleTree
hintRuleSelection typeSystem path tree =
    let
        (Node { rule } _) =
            Debug.log "hintTree:" <| doHint typeSystem tree
    in
    setRule path rule tree


doHint : TypeSystem -> RuleTree -> RuleTree
doHint typeSystem ((Node ({ ctx, term } as content) _) as t1) =
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
    in
    case ( maybeCtx, maybeTerm ) of
        ( Just justCtx, Just justTerm ) ->
            inferTree typeSystem justCtx justTerm
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


doSubstitution : Substitutor.Model.Model -> RuleTree -> RuleTree
doSubstitution sm tree =
    case ( parsedType sm, parsedVar sm ) of
        ( Ok tyS, Ok varS ) ->
            let
                ss =
                    [ ( tyS, varS ) ]
            in
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
