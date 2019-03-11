module Utils.Tree exposing (..)

import List.Extra


type Tree a
    = Node a (List (Tree a))


map : (a -> b) -> Tree a -> Tree b
map f t =
    case t of
        Node x nodes ->
            Node (f x) (List.map (map f) nodes)


zipWith : (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWith f t1 t2 =
    case ( t1, t2 ) of
        ( Node x1 ns1, Node x2 ns2 ) ->
            Node (f x1 x2) (List.Extra.lift2 (zipWith f) ns1 ns2)


zipWithExtra : (a -> Tree a -> b -> Tree b -> c) -> Tree a -> Tree b -> Tree c
zipWithExtra f t1 t2 =
    case ( t1, t2 ) of
        ( Node x1 ns1, Node x2 ns2 ) ->
            Node (f x1 t1 x2 t2) (List.Extra.lift2 (zipWithExtra f) ns1 ns2)


zip : Tree a -> Tree b -> Tree ( a, b )
zip t1 t2 =
    zipWith (\a b -> ( a, b )) t1 t2
