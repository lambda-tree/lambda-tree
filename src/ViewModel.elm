module ViewModel exposing (..)

import Lambda.Parse exposing (parseCtx, parseTerm, parseType)
import Lambda.Rule exposing (tryRule)
import Model exposing (Rule, Tree(..), TreeModel)


type alias TreeViewDataError =
    { row : Int, col : Int }


type alias TreeViewDataResult =
    { text : String, error : Maybe TreeViewDataError }


type alias TreeViewData =
    Tree
        { ctx : TreeViewDataResult
        , term : TreeViewDataResult
        , ty : TreeViewDataResult
        , rule : Rule
        , result : String
        }


getTreeViewData : TreeModel -> TreeViewData
getTreeViewData t =
    case t of
        Node { ctx, term, ty, rule } children ->
            let
                parsedCtx =
                    parseCtx ctx

                parsedTerm =
                    parseTerm term

                parsedTy =
                    parseType ty

                extractError result =
                    case result of
                        Err e ->
                            Just e

                        Ok _ ->
                            Nothing
            in
            Node
                { ctx = { text = ctx, error = extractError parsedCtx }
                , term = { text = term, error = extractError parsedTerm }
                , ty = { text = ty, error = extractError parsedTy }
                , rule = rule
                , result = tryRule t
                }
            <|
                List.map getTreeViewData children
