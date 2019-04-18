module Message exposing (..)

import Model exposing (..)
import Substitutor.Message


type Msg
    = ZoomIn
    | ZoomOut
    | TextChangedMsg (List Int) TextKind String
    | ClickedMsg
    | AddMsg (List Int)
    | RemoveMsg (List Int)
    | RuleSelectedMsg (List Int) Rule
    | SubstitutionMsg Substitutor.Message.Msg
    | DoSubstitutionMsg
