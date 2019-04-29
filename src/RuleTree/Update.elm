module RuleTree.Update exposing (..)

import Inferer.Inferer exposing (buildTree)
import Lambda.ExpressionUtils exposing (substFtv, substFtvCtx, substFtvTerm)
import Lambda.Parse exposing (parseCtx, parseTerm, parseType)
import Lambda.ParseTransform exposing (fromParseContext, fromParseTerm, fromParseType)
import Lambda.Show exposing (showCtx, showTerm, showType)
import List.Extra
import RuleTree.Message exposing (..)
import RuleTree.Model exposing (..)
import Substitutor.Model
import Substitutor.Utils exposing (parsedType, parsedVar)
import Utils.Tree exposing (Tree(..))


update : Msg -> RuleTree -> RuleTree
update msg tree =
    case Debug.log "update :: Msg" msg of
        TextChangedMsg path kind text ->
            updateTextInPath kind tree text path

        HintMsg ->
            doHint tree

        RemoveMsg path ->
            setRule path NoRule tree

        RuleSelectedMsg path rule ->
            setRule path rule tree

        DoSubstitutionMsg s ->
            doSubstitution s tree

        RuleClickedMsg _ ->
            Debug.todo "Update: RuleClickedMsg"

        SelectRuleMsg _ ->
            Debug.todo "Update: SelectRuleMsg"


doHint : RuleTree -> RuleTree
doHint ((Node { ctx, term } _) as t1) =
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
            buildTree justCtx justTerm
                |> Result.mapError (Debug.log "doHint: buildTree error:")
                |> Result.toMaybe
                |> Maybe.map
                    (Utils.Tree.map
                        (\content ->
                            { ctx = showCtx content.ctx
                            , term = showTerm content.ctx content.term
                            , ty = showType justCtx content.ty
                            , rule = content.rule
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
                                    |> Maybe.map Lambda.Show.showCtx
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
                                                |> Maybe.map (Lambda.Show.showTerm justCtx)
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
                                                                substFtv ss justTy
                                                        in
                                                        if substituted == justTy then
                                                            Nothing

                                                        else
                                                            Just substituted
                                                    )
                                                |> Maybe.map (Lambda.Show.showType justCtx)
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
                            Node { content | rule = rule } [ emptyTree ]

                        TVarInst ->
                            Node { content | rule = rule } [ emptyTree ]

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


updateTextInPath kind tree text path =
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
                                        updateTextInPath kind t text subPath

                                    else
                                        t
                                )
                                children
                    in
                    Node content updatedChildren
