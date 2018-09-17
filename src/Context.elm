module Context exposing (..)


emptycontext =
    []


ctxlength ctx =
    List.length ctx


addbinding ctx x bind =
    ( x, bind ) :: ctx


addname ctx x =
    addbinding ctx x NameBind


isnamebound ctx x =
    case ctx of
        [] ->
            False

        ( y, _ ) :: rest ->
            y == x || isnamebound rest x


pickfreshname ctx x =
    if isnamebound ctx x then
        pickfreshname ctx (x ++ "'")
    else
        ( (( x, NameBind ) :: ctx), x )


index2name fi ctx x =
    case x of
        0 ->
            case ctx of
                [] ->
                    Nothing

                ( y, _ ) :: rest ->
                    y

        n ->
            case ctx of
                [] ->
                    Nothing

                _ :: rest ->
                    index2name fi rest x


name2index fi ctx x =
    case ctx of
        [] ->
            Nothing

        ( y, _ ) :: rest ->
            if y == x then
                0
            else
                1 + (name2index fi rest x)
