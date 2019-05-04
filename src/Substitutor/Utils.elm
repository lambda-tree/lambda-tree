module Substitutor.Utils exposing (..)

import Lambda.ContextUtils exposing (emptycontext)
import Lambda.Expression
import Lambda.Parse exposing (parseType, parseTypeVar)
import Lambda.ParseTransform exposing (fromParseType)
import Result exposing (Result)
import Result.Extra
import Substitutor.Model


parsedType : Substitutor.Model.Model -> Result Lambda.Parse.Error Lambda.Expression.Ty
parsedType m =
    parseType m.ty
        |> Result.map (fromParseType emptycontext)


parsedVar : Substitutor.Model.Model -> Result Lambda.Parse.Error String
parsedVar m =
    parseTypeVar m.var


isValid : Substitutor.Model.Model -> Bool
isValid m =
    Result.Extra.isOk (parsedType m) && Result.Extra.isOk (parsedVar m)
