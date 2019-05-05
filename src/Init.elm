module Init exposing (..)

import Bootstrap.Dropdown as Dropdown
import Decode
import ErrorReport.Init
import Json.Decode as D
import Model exposing (Model)
import Ports exposing (log)
import RuleTree.Init
import Settings.Init
import Substitutor.Init
import Update exposing (updateWithDecodeModel)


defaultModel : Model
defaultModel =
    { ruleTree = RuleTree.Init.init
    , substitution = Substitutor.Init.init
    , settings = Settings.Init.init
    , errorReport = ErrorReport.Init.init
    , exportDropdown = Dropdown.initialState
    }


init : D.Value -> ( Model, Cmd msg )
init cached =
    case D.decodeValue (D.nullable Decode.modelDecoder) cached of
        Ok (Just decodeModel) ->
            ( updateWithDecodeModel decodeModel defaultModel, Cmd.none )

        Ok Nothing ->
            ( defaultModel, Cmd.none )

        Err e ->
            ( defaultModel, log <| "Parsing init flags failed: " ++ D.errorToString e )
