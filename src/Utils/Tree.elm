module Utils.Tree exposing (..)

import Json.Decode as D
import Json.Encode as E
import List.Extra


type Tree a
    = Node a (List (Tree a))


map : (a -> b) -> Tree a -> Tree b
map f t =
    case t of
        Node x nodes ->
            Node (f x) (List.map (map f) nodes)


indexedMap : (List Int -> a -> b) -> Tree a -> Tree b
indexedMap =
    let
        indexedMapWithPath path f t =
            case t of
                Node x nodes ->
                    Node (f path x) (List.indexedMap (\i -> indexedMapWithPath (path ++ [ i ]) f) nodes)
    in
    indexedMapWithPath []


zipWith : (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWith f t1 t2 =
    case ( t1, t2 ) of
        ( Node x1 ns1, Node x2 ns2 ) ->
            Node (f x1 x2) (listZipWith (zipWith f) ns1 ns2)


zipWithExtra : (a -> Tree a -> b -> Tree b -> c) -> Tree a -> Tree b -> Tree c
zipWithExtra f t1 t2 =
    case ( t1, t2 ) of
        ( Node x1 ns1, Node x2 ns2 ) ->
            Node (f x1 t1 x2 t2) (listZipWith (zipWithExtra f) ns1 ns2)


zip : Tree a -> Tree b -> Tree ( a, b )
zip t1 t2 =
    zipWith (\a b -> ( a, b )) t1 t2


listZipWith : (a -> b -> c) -> List a -> List b -> List c
listZipWith f l1 l2 =
    case ( l1, l2 ) of
        ( h1 :: t1, h2 :: t2 ) ->
            f h1 h2 :: listZipWith f t1 t2

        _ ->
            []


mapContentAtPath : List Int -> (a -> a) -> Tree a -> Tree a
mapContentAtPath path f (Node content children) =
    case path of
        [] ->
            Node (f content) children

        idx :: subPath ->
            Node content (children |> List.Extra.updateAt idx (mapContentAtPath subPath f))


foldr : (a -> b -> b) -> b -> Tree a -> b
foldr f acc (Node content children) =
    f content <| List.foldr (\t1 r -> foldr f r t1) acc children


foldTree : (a -> List b -> b) -> Tree a -> b
foldTree f (Node content children) =
    f content (List.map (foldTree f) children)


toList : Tree a -> List a
toList =
    foldr (::) []


encodeTree : (a -> E.Value) -> Tree a -> E.Value
encodeTree encodeContent =
    foldTree
        (\content childrenValues ->
            E.object
                [ ( "content", encodeContent content )
                , ( "children", E.list identity childrenValues )
                ]
        )


decodeTree : D.Decoder a -> D.Decoder (Tree a)
decodeTree decodeContent =
    D.map2 Node
        (D.field "content" decodeContent)
        (D.field "children" <| D.list <| D.lazy (\_ -> decodeTree decodeContent))
