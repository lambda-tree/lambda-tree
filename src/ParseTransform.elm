module ParseTransform exposing (..)

import List.Extra exposing (mapAccuml)
import Maybe.Extra
import Parse as P
import Lambda exposing (..)
import Context exposing (..)
import ContextUtils exposing (..)
import Either exposing (Either(..))


type ParseError
    = IndexNotFound String


{-| Transforms Parsed context to lambda expression context
Note that Parse type context is ordered "left to right" but lambda Context is ordered "right to left"
-}
fromParseContext : P.TyContext -> Context
fromParseContext ctx =
    case ctx of
        P.TyContext bindings ->
            bindings
                |> mapAccuml
                    (\resolvedCtx binding ->
                        let
                            transformed =
                                case binding of
                                    P.VarBind name maybeType ->
                                        ( name
                                        , maybeType
                                            |> Maybe.map (fromParseType resolvedCtx)
                                            -- TODO: Handle the error!!
                                            |> Maybe.map Either.toMaybe
                                            |> Maybe.Extra.join
                                            |> Maybe.map VarBind
                                            |> Maybe.withDefault NameBind
                                        )

                                    P.TyVarBind name ->
                                        ( name, TyVarBind )
                        in
                            ( addbindingTup resolvedCtx transformed, transformed )
                    )
                    []
                |> Tuple.first


fromParseType : Context -> P.Ty -> Either ParseError Ty
fromParseType ctx ty =
    case ty of
        P.TyVar name ->
            case name2index I ctx name of
                Just index ->
                    Right <| TyVar index (ctxlength ctx)

                Nothing ->
                    Left (IndexNotFound name)

        P.TyArr ty1 ty2 ->
            fromParseType ctx ty1
                |> Either.andThenRight
                    (\t1 ->
                        fromParseType ctx ty2
                            |> Either.andThenRight
                                (\t2 -> Right <| TyArr t1 t2)
                    )

        P.TyAll name ty1 ->
            fromParseType (addbinding ctx name TyVarBind) ty1
                |> Either.mapRight (TyAll name)
