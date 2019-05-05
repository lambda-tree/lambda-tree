module Settings.Utils exposing (..)

import Lambda.Expression
import Settings.Model exposing (Model)


getTypeSystem : Model -> Lambda.Expression.TypeSystem
getTypeSystem model =
    model.typeSystem


setTypeSystem : Lambda.Expression.TypeSystem -> Model -> Model
setTypeSystem typeSystem model =
    { model | typeSystem = typeSystem }
