module Lambda.ContextUtils exposing (..)

import Lambda.Context exposing (..)
import Lambda.Expression exposing (..)
import List.Extra exposing (find, findIndex)
import Maybe.Extra exposing (isJust)


emptycontext : Context
emptycontext =
    []


ctxlength : Context -> Int
ctxlength ctx =
    List.length ctx


addbinding : Context -> String -> Binding -> Context
addbinding ctx x bind =
    ( x, bind ) :: ctx


popbinding : Context -> Context
popbinding ctx =
    List.tail ctx
        |> Maybe.withDefault emptycontext


addbindingTup : Context -> ( String, Binding ) -> Context
addbindingTup ctx tup =
    case tup of
        ( x, bind ) ->
            addbinding ctx x bind


addname : Context -> String -> Context
addname ctx x =
    addbinding ctx x NameBind


isnamebound : Context -> String -> Bool
isnamebound ctx x =
    ctx
        |> find (Tuple.first >> (==) x)
        |> isJust


pickfreshname : Context -> String -> ( Context, String )
pickfreshname ctx x =
    if isnamebound ctx x then
        pickfreshname ctx (x ++ "'")

    else
        ( ( x, NameBind ) :: ctx, x )


index2name : Info -> Context -> Int -> Maybe String
index2name fi ctx x =
    ctx
        |> List.Extra.getAt x
        |> Maybe.map Tuple.first


getbinding : Context -> Int -> Maybe Binding
getbinding ctx x =
    ctx
        |> List.Extra.getAt x
        |> Maybe.map Tuple.second


name2index : Info -> Context -> String -> Maybe Int
name2index fi ctx x =
    ctx
        |> findIndex (Tuple.first >> (==) x)


getTypeFromContext : Info -> Context -> Int -> Maybe Ty
getTypeFromContext fi ctx i =
    case getbinding ctx i of
        Just (VarBind ty) ->
            Just ty

        _ ->
            Nothing
