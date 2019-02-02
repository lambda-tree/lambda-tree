module Main exposing (..)

import Browser
import Debug
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Css exposing (..)
import Css.Global exposing (global, body, selector)
import Html.Styled as S exposing (Html, Attribute, styled, toUnstyled, Attribute, div)
import Parse
import List.Extra


-- CSS Constants


cmFont =
    fontFamilies [ "cm" ]



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }



-- MODEL


type Rule
    = RuleAbs
    | RuleApp
    | RuleNothing


type alias NodeContent =
    { ctx : String, term : String, ty : String, rule : Rule }


type Tree
    = Node NodeContent (List Tree)


exampleContent =
    { ctx = "TypeVar1, termVar1: TypeVar1, TypeVar2, termVar2: TypeVar1"
    , term = "(λ x: TypeVar1. x) termVar2"
    , ty = "TypeVar1"
    , rule = RuleNothing
    }


exampleTree : Tree
exampleTree =
    Node exampleContent
        [ Node exampleContent
            [ Node exampleContent
                [ Node exampleContent
                    [ Node exampleContent []
                    , Node exampleContent []
                    ]
                ]
            , Node exampleContent
                [ Node exampleContent []
                ]
            ]
        ]


emptyTree : Tree
emptyTree =
    Node { ctx = "", term = "", ty = "", rule = RuleNothing } []


type alias Model =
    { tree : Tree }


init : Model
init =
    { tree = emptyTree }



-- UPDATE


type TextKind
    = CtxKind
    | TermKind
    | TyKind


type Msg
    = TextChangedMsg (List Int) TextKind String
    | ClickedMsg
    | AddMsg (List Int)
    | RemoveMsg (List Int)


update : Msg -> Model -> Model
update msg model =
    case Debug.log "update :: Msg" msg of
        TextChangedMsg path kind text ->
            { model | tree = updateTextInPath kind model.tree text path }

        ClickedMsg ->
            model

        AddMsg path ->
            { model | tree = addNode path model.tree }

        RemoveMsg path ->
            { model | tree = removeNode path model.tree }


addNode : List Int -> Tree -> Tree
addNode path tree =
    case tree of
        Node content children ->
            case path of
                [] ->
                    Node content (emptyTree :: children)

                idx :: subPath ->
                    let
                        updatedChildren =
                            List.indexedMap
                                (\i t ->
                                    if i == idx then
                                        addNode subPath t
                                    else
                                        t
                                )
                                children
                    in
                        Node content updatedChildren


removeNode : List Int -> Tree -> Tree
removeNode path tree =
    case tree of
        Node content children ->
            case path of
                [] ->
                    Node content children

                idx :: [] ->
                    Node content (List.Extra.removeAt idx children)

                idx :: subPath ->
                    let
                        updatedChildren =
                            List.indexedMap
                                (\i t ->
                                    if i == idx then
                                        removeNode subPath t
                                    else
                                        t
                                )
                                children
                    in
                        Node content updatedChildren


updateTextInPath kind tree text path =
    let
        preprocessed =
            Parse.preprocess text
    in
        case tree of
            Node content children ->
                case path of
                    [] ->
                        case kind of
                            CtxKind ->
                                Node { content | ctx = preprocessed } children

                            TermKind ->
                                Node { content | term = preprocessed } children

                            TyKind ->
                                Node { content | ty = preprocessed } children

                    idx :: subPath ->
                        let
                            updatedChildren =
                                List.indexedMap
                                    (\i t ->
                                        if i == idx then
                                            updateTextInPath kind t text subPath
                                        else
                                            t
                                    )
                                    children
                        in
                            Node content updatedChildren



-- VIEW


theme :
    { background : Color
    , text : Color
    , secondary : Color
    , line : Color
    , darkLine : Color
    , clear : Color
    , inputBackground : Color
    }
theme =
    { background = rgb 240 240 240
    , text = rgba 10 10 10 0.85
    , secondary = rgb 250 240 230
    , line = rgb 211 211 211
    , darkLine = rgb 150 150 150
    , clear = rgba 0 0 0 0
    , inputBackground = rgba 255 255 255 0.5
    }


background : List (Attribute msg) -> List (Html msg) -> Html msg
background =
    styled div
        [ backgroundColor theme.background
        , height <| pct 100
        ]


lambdaExprInput alignRight value onInput =
    styled S.input
        [ color <| theme.text
        , backgroundColor <| theme.inputBackground
        , fontSize <| rem 1
        , cmFont
        , width <| pct 100
        , padding2 (rem 0.3) (rem 0.6)
        , borderRadius <| rem 0.5
        , borderWidth <| rem 0.1
        , borderStyle solid
        , border <| rem 0
        , property "-webkit-appearance" "none"
        , property "-moz-appearance" "none"
        , property "-ms-appearance" "none"
        , property "-o-appearance" "none"
        , property "appearance" "none"
        , outline none
        , boxShadow none
        , property "direction" <|
            if alignRight then
                "ltr"
            else
                "ltr"
        ]
        [ A.value value, E.onInput onInput ]
        []


lambdaExprText value =
    styled S.div
        [ color theme.text
        , backgroundColor theme.clear
        , fontSize <| rem 1
        , cmFont
        , fontWeight normal
        , margin2 (rem 0) (rem 0.3)
        ]
        []
        [ S.text value ]


proofCell : NodeContent -> (TextKind -> String -> Msg) -> S.Html Msg
proofCell content msgCreator =
    S.div
        []
        [ styled S.div
            [ displayFlex, flexShrink <| int 0, alignItems center, minWidth <| rem 45, margin <| rem 0.5 ]
            []
            [ styled S.div [ displayFlex, flex <| int 1 ] [] [ lambdaExprInput True content.ctx (msgCreator CtxKind) ]
            , lambdaExprText "⊢"
            , styled S.div [ displayFlex, flex <| int 2 ] [] [ lambdaExprInput False content.term (msgCreator TermKind) ]
            , lambdaExprText ":"
            , styled S.div [ displayFlex, flex <| int 1 ] [] [ lambdaExprInput False content.ty (msgCreator TyKind) ]
            ]
        ]


view : Model -> Html Msg
view model =
    styled S.div
        [ backgroundColor theme.background
        , height <| pct 100
        , minHeight <| pct 100
        ]
        []
        [ global
            [ Css.Global.html [ height <| pct 100, minHeight <| pct 100 ]
            , body [ height <| pct 100, minHeight <| pct 100, backgroundColor theme.background, margin <| px 0 ]
            , selector "@font-face"
                [ property "src" "url(resources/fonts/cmunrm.ttf)"
                , fontStyle normal
                , cmFont
                ]
            , selector "@font-face"
                [ property "src" "url(resources/fonts/cmunti.ttf)"
                , fontStyle italic
                , cmFont
                ]
            ]
        , mainContent model
        ]


mainContent model =
    styled S.div
        [ displayFlex
        , flex <| auto
        , alignItems <| stretch
        , justifyContent stretch
        , height <| pct 100
        , minHeight <| pct 100
        ]
        []
        [ leftColumn
            [ treeContainer
                [ drawTree False model.tree
                ]
            ]
        , rightColumn
            [ scroller
                [ styled S.button [ margin <| rem 0.5 ] [ E.onClick ClickedMsg ] [ S.text "Click" ]
                ]
            ]
        ]


leftColumn children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 8
        , alignItems flexStart
        , justifyContent flexStart
        , overflow auto
        , whiteSpace noWrap
        ]
        []
        children


rightColumn children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 1
        , alignItems stretch
        , justifyContent flexStart
        , overflow auto
        , whiteSpace noWrap
        , borderStyle solid
        , borderWidth4 (px 0) (px 0) (px 0) (px 1)
        , borderColor <| theme.line
        ]
        []
        children


treeContainer children =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems center
        ]
        []
        children


scroller children =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems center
        , overflow scroll
        ]
        []
        children


drawTree drawLineBelow tree =
    let
        drawTreeP dlb t path =
            case t of
                Node content children ->
                    styled S.div
                        [ displayFlex, flexDirection column, alignItems center, padding2 (rem 0) (rem 1.5) ]
                        []
                        [ if List.length children == 0 then
                            S.div []
                                [ S.button [ E.onClick <| AddMsg path ] [ S.text "Add" ]
                                , S.button [ E.onClick <| RemoveMsg path ] [ S.text "Remove" ]
                                ]
                          else
                            S.div [] []
                        , styled S.div
                            [ displayFlex, margin2 (rem 0) (rem -1.5), alignItems flexEnd ]
                            []
                            (children
                                |> List.indexedMap (\i t1 -> drawTreeP (List.length children <= 1) t1 (path ++ [ i ]))
                            )
                        , if List.length children <= 1 then
                            S.div [] []
                          else
                            hairline
                        , styled S.div
                            [ displayFlex, flexDirection column, alignItems stretch ]
                            []
                            [ proofCell content (TextChangedMsg path)
                            , if dlb then
                                hairline
                              else
                                S.div [] []
                            ]
                        ]
    in
        drawTreeP drawLineBelow tree []


hairline =
    styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.darkLine, width <| pct 100 ] [] []
