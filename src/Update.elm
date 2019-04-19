module Update exposing (..)

import Lambda.ExpressionUtils exposing (substFtv, substFtvCtx, substFtvTerm)
import Lambda.Parse exposing (parseCtx, parseTerm, parseType)
import Lambda.ParseTransform exposing (fromParseContext, fromParseTerm, fromParseType)
import Lambda.Show
import List.Extra
import Message exposing (..)
import Model exposing (..)
import Substitutor.Init
import Substitutor.Model
import Substitutor.Update
import Substitutor.Utils exposing (parsedType, parsedVar)
import Utils.Tree exposing (Tree(..))


update : Msg -> Model -> Model
update msg model =
    case Debug.log "update :: Msg" msg of
        TextChangedMsg path kind text ->
            { model | tree = updateTextInPath kind model.tree text path }

        ClickedMsg ->
            model

        AddMsg path ->
            { model | tree = addNode path model.tree NoRule }

        RemoveMsg path ->
            { model | tree = removeNode path model.tree }

        RuleSelectedMsg path rule ->
            { model | tree = addNode path model.tree rule }

        ZoomIn ->
            { model | zoomLevel = model.zoomLevel * 1.2 }

        ZoomOut ->
            { model | zoomLevel = model.zoomLevel / 1.2 }

        SubstitutionMsg m ->
            { model | substitution = Substitutor.Update.update m model.substitution }

        DoSubstitutionMsg ->
            { model | tree = doSubstitution model.substitution model.tree, substitution = Substitutor.Init.init }


doSubstitution : Substitutor.Model.Model -> TreeModel -> TreeModel
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


addNode : List Int -> TreeModel -> Rule -> TreeModel
addNode path tree rule =
    case tree of
        Node content children ->
            case path of
                [] ->
                    case rule of
                        TTrue ->
                            Node { content | rule = rule } [ emptyTree ]

                        TFalse ->
                            Node { content | rule = rule } [ emptyTree ]

                        TVar ->
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

                        TGen ->
                            Node { content | rule = rule } [ emptyTree ]

                        TInst ->
                            Node { content | rule = rule } [ emptyTree ]

                        NoRule ->
                            Node { content | rule = rule } [ emptyTree ]

                idx :: subPath ->
                    let
                        updatedChildren =
                            List.indexedMap
                                (\i t ->
                                    if i == idx then
                                        addNode subPath t rule

                                    else
                                        t
                                )
                                children
                    in
                    Node content updatedChildren


removeNode : List Int -> TreeModel -> TreeModel
removeNode path tree =
    case tree of
        Node content children ->
            case path of
                [] ->
                    Node content []

                idx :: subPath ->
                    Node content (children |> List.Extra.updateAt idx (removeNode subPath))


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
