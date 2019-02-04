module Model exposing (..)

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
    | NoRule


type alias ModelContent =
    { ctx : String, term : String, ty : String, rule : Rule }


type Tree a
    = Node a (List (Tree a))


type alias TreeModel =
    Tree ModelContent



--
--exampleContent : ModelContent
--exampleContent =
--    { ctx = "TypeVar1, termVar1: TypeVar1, TypeVar2, termVar2: TypeVar1"
--    , term = "(λ x: TypeVar1. x) termVar2"
--    , ty = "TypeVar1"
--    , rule = NoRule
--    }
--
--
--exampleTree : TreeModel
--exampleTree =
--    Node exampleContent
--        [ Node exampleContent
--            [ Node exampleContent
--                [ Node exampleContent
--                    [ Node exampleContent []
--                    , Node exampleContent []
--                    ]
--                ]
--            , Node exampleContent
--                [ Node exampleContent []
--                ]
--            ]
--        ]


emptyTree : TreeModel
emptyTree =
    Node { ctx = "", term = "", ty = "", rule = NoRule } []


type alias Model =
    { tree : TreeModel, zoomLevel : Float }


type TextKind
    = CtxKind
    | TermKind
    | TyKind
