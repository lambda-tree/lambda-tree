module Update exposing (..)

import Lambda.Parse
import List.Extra
import Message exposing (..)
import Model exposing (..)
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
                    Node content children

                idx :: [] ->
                    Node content (List.Extra.removeAt idx children)

                idx :: subPath ->
                    let
                        updatedChildren =
                            List.indexedMap
                                (\i t ->
                                    if i == idx then
                                        removeNode subPath t

                                    else
                                        t
                                )
                                children
                    in
                    Node content updatedChildren


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
