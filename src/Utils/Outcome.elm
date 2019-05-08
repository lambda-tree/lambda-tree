module Utils.Outcome exposing (..)

import Maybe.Extra
import Result.Extra


type Outcome e v
    = Outcome (Maybe e) v


fine : v -> Outcome e v
fine val =
    Outcome Nothing val


problem : e -> v -> Outcome e v
problem err val =
    Outcome (Just err) val


fromResult : v -> Result e v -> Outcome e v
fromResult val =
    Result.mapError (\e -> Outcome (Just e) val) >> Result.map fine >> Result.Extra.merge


join : Outcome e (Outcome e v) -> Outcome e v
join (Outcome e1 (Outcome e2 v)) =
    Outcome (Maybe.Extra.or e1 e2) v


toResult : Outcome e v -> Result e v
toResult (Outcome err val) =
    case err of
        Just e ->
            Result.Err e

        Nothing ->
            Result.Ok val


map : (v -> v1) -> Outcome e v -> Outcome e v1
map f (Outcome err val) =
    let
        val2 =
            f val
    in
    Outcome err val2


andThen : (v -> Outcome e v1) -> Outcome e v -> Outcome e v1
andThen f (Outcome err val) =
    let
        (Outcome err1 val2) =
            f val
    in
    Outcome (Maybe.Extra.or err err1) val2


value : Outcome e v -> v
value (Outcome _ v) =
    v


error : Outcome e v -> Maybe e
error (Outcome e _) =
    e
