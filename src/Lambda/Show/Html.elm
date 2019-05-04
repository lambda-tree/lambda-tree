module Lambda.Show.Html exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Lambda.Show.Print exposing (..)


show : Print -> S.Html msg
show print =
    case print of
        Var s ->
            it s

        TypeVar s ->
            txt s

        Const s ->
            txt s

        TypeConst s ->
            txt s

        SubExpr s ->
            it s

        OfType p1 p2 ->
            g [ show p1, txt " : ", show p2 ]

        ElemOf p1 p2 ->
            g [ show p1, txt " ∈ ", show p2 ]

        Imply p1 p2 ->
            g [ show p1, styled S.span [ padding2 (px 0) (em 0.5) ] [] [ txt "⊢" ], show p2 ]

        Prime p1 ->
            g [ show p1, it "'" ]

        Appl p1 p2 ->
            g [ show p1, txt " ", show p2 ]

        Subset p1 p2 ->
            g [ show p1, txt " ⊑ ", show p2 ]

        NotElem p1 p2 ->
            g
                [ show p1
                , txt " "
                , styled S.span
                    [ position relative ]
                    []
                    [ txt "∈"
                    , styled S.span [ position absolute, left <| em 0.1 ] [] [ txt "/" ]
                    ]
                , txt " "
                , show p2
                ]

        FV p1 ->
            g [ it "FV", txt "(", show p1, txt ")" ]

        Forall p1 p2 ->
            g [ txt "∀", show p1, halfSpaced ". ", show p2 ]

        Arrow p1 p2 ->
            g [ show p1, halfSpaced " → ", show p2 ]

        Abs p1 p2 ->
            g [ txt "λ", show p1, halfSpaced ". ", show p2 ]

        IfThenElse p1 p2 p3 ->
            g [ txt "if ", show p1, txt " then ", show p2, txt " else ", show p3 ]

        ArrowOver p1 ->
            styled S.span
                [ position relative ]
                []
                [ show p1
                , styled S.span [ position absolute, left <| em 0.15, top <| em 0, fontSize <| em 0.5, fontWeight bold ] [] [ txt "→" ]
                ]

        Let p1 p2 p3 ->
            g [ txt "let ", show p1, txt " = ", show p2, txt " in ", show p3 ]

        AddToCtx p1 p2 ->
            g [ show p1, txt ", ", show p2 ]

        SubstType p1 p2 p3 ->
            g [ txt "{", show p1, txt "/", show p2, txt "}", show p3 ]

        TypeAppl p1 p2 ->
            g [ show p1, txt " [", show p2, txt "]" ]

        TypeAbs p1 p2 ->
            g [ txt "Λ", show p1, halfSpaced ". ", show p2 ]

        Bracket p1 ->
            g [ txt "(", show p1, txt ")" ]

        Sequence ps ->
            ps
                |> List.map show
                |> List.intersperse (txt ", ")
                |> g

        Gamma ->
            txt "Γ"

        Sigma ->
            txt "σ"

        Tau ->
            txt "τ"

        Alpha ->
            txt "α"


g =
    S.span []


it text =
    styled S.span [ fontStyle italic ] [] [ S.text text ]


txt text =
    S.text text


halfSpace =
    styled S.span [ marginRight <| em 0.1 ] [] []


halfSpaced text =
    styled S.span [ property "word-spacing" "-0.15em" ] [] [ S.text text ]
