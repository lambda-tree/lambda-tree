module Lambda.TypeReturn exposing (..)

import Lambda.Context exposing (Context)
import Lambda.Expression exposing (Ty)


type alias TypeReturn =
    { ty : Ty, ctx : Context }


mapTy : (Ty -> Ty) -> TypeReturn -> TypeReturn
mapTy f r =
    { r | ty = f r.ty }
