module ContextUtils exposing (..)

import Context exposing (..)
import Lambda exposing (..)
import List.Extra


emptycontext : Context
emptycontext =
    []


ctxlength : Context -> Int
ctxlength ctx =
    List.length ctx


addbinding : Context -> String -> Binding -> Context
addbinding ctx x bind =
    ( x, bind ) :: ctx



--getbinding : Context -> Int -> Maybe Binding
--getbinding ctx x =
--    List.Extra.getAt (ctxlength ctx - x) ctx
--


addname : Context -> String -> Context
addname ctx x =
    addbinding ctx x NameBind


isnamebound : Context -> String -> Bool
isnamebound ctx x =
    case ctx of
        [] ->
            False

        ( y, _ ) :: rest ->
            y == x || isnamebound rest x


pickfreshname : Context -> String -> ( Context, String )
pickfreshname ctx x =
    if isnamebound ctx x then
        pickfreshname ctx (x ++ "'")
    else
        ( (( x, NameBind ) :: ctx), x )


index2name : Info -> Context -> Int -> Maybe String
index2name fi ctx x =
    case x of
        0 ->
            case ctx of
                [] ->
                    Nothing

                ( y, _ ) :: rest ->
                    Just y

        n ->
            case ctx of
                [] ->
                    Nothing

                _ :: rest ->
                    index2name fi rest x


getbinding : Context -> Int -> Maybe Binding
getbinding ctx x =
    case x of
        0 ->
            case ctx of
                [] ->
                    Nothing

                ( _, y ) :: rest ->
                    Just y

        n ->
            case ctx of
                [] ->
                    Nothing

                _ :: rest ->
                    getbinding rest x


name2index : Info -> Context -> String -> Maybe Int
name2index fi ctx x =
    case ctx of
        [] ->
            Nothing

        ( y, _ ) :: rest ->
            if y == x then
                Just 0
            else
                Maybe.map ((+) 1) (name2index fi rest x)
