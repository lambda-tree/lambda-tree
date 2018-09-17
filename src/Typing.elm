module Typing exposing (..)

import Lambda exposing (..)


{-| Deduce type of term `t` in context `ctx`
-}
typeof ctx t =
    case t of
        TmVar fi x n ->
            onvar fi c x n

        TmAbs fi x tyT1 t2 ->
            TmAbs fi x (ontype c tyT1) (walk (c + 1) t2)

        TmApp fi t1 t2 ->
            TmApp fi (walk c t1) (walk c t2)

        TmTAbs fi tyX t2 ->
            TmTAbs fi tyX (walk (c + 1) t2)

        TmTApp fi t1 tyT2 ->
            TmTApp fi (walk c t1) (ontype c tyT2)
