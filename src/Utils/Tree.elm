module Utils.Tree exposing (..)


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
