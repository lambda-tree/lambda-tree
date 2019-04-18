module Model exposing (..)

import Substitutor.Model
import Utils.Tree exposing (Tree(..))


{-| The data representation for the single source of truth in the App.
The view would ideally transform it to some "view model" on every render. That would be inefficient though, so the model is fragmented and there is a view model for every fragment.
-}
type Rule
    = TTrue
    | TFalse
    | TVar
    | TAbs
    | TApp
    | TIf
    | TTAbs
    | TTApp
    | TLet
    | TGen
    | TInst
    | NoRule


type alias ModelContent =
    { ctx : String, term : String, ty : String, rule : Rule }


type alias TreeModel =
    Tree ModelContent


emptyTree : TreeModel
emptyTree =
    Node { ctx = "", term = "", ty = "", rule = NoRule } []


type alias Model =
    { tree : TreeModel, zoomLevel : Float, substitution : Substitutor.Model.Model }


type TextKind
    = CtxKind
    | TermKind
    | TyKind
