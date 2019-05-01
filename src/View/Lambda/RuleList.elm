module View.Lambda.RuleList exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Lambda.Rule exposing (Rule(..))
import Lambda.Show exposing (showRule)
import View.Theme exposing (theme)


type Print
    = Var String
    | Const String
    | TyVar String
    | SubExpr String
    | OfType Print Print
    | ElemOf Print Print
    | Imply Print Print
    | Prime Print
    | Appl Print Print
    | Subset Print Print
    | NotElem Print Print
    | FV Print
    | Forall Print Print
    | Arrow Print Print
    | Abs Print Print
    | IfThenElse Print Print Print
    | ArrowOver Print
    | Let Print Print Print
    | AddToCtx Print Print
    | SubstType Print Print Print
    | TypeAppl Print Print
    | TypeAbs Print Print
    | Gamma
    | Sigma
    | Tau
    | Alpha


type alias RuleTemplate =
    { tops : List Print, bottom : Print }


showPrint : Print -> S.Html msg
showPrint print =
    case print of
        Var s ->
            it s

        Const s ->
            txt s

        TyVar s ->
            txt s

        SubExpr s ->
            it s

        OfType p1 p2 ->
            g [ showPrint p1, txt " : ", showPrint p2 ]

        ElemOf p1 p2 ->
            g [ showPrint p1, txt " ∈ ", showPrint p2 ]

        Imply p1 p2 ->
            g [ showPrint p1, styled S.span [ padding2 (px 0) (em 0.5) ] [] [ txt "⊢" ], showPrint p2 ]

        Prime p1 ->
            g [ showPrint p1, it "'" ]

        Appl p1 p2 ->
            g [ showPrint p1, txt " ", showPrint p2 ]

        Subset p1 p2 ->
            g [ showPrint p1, txt " ⊑ ", showPrint p2 ]

        NotElem p1 p2 ->
            g
                [ showPrint p1
                , txt " "
                , styled S.span
                    [ position relative ]
                    []
                    [ txt "∈"
                    , styled S.span [ position absolute, left <| em 0.1 ] [] [ txt "/" ]
                    ]
                , txt " "
                , showPrint p2
                ]

        FV p1 ->
            g [ it "FV", txt "(", showPrint p1, txt ")" ]

        Forall p1 p2 ->
            g [ txt "∀", showPrint p1, halfSpaced ". ", showPrint p2 ]

        Arrow p1 p2 ->
            g [ showPrint p1, halfSpaced " → ", showPrint p2 ]

        Abs p1 p2 ->
            g [ txt "λ", showPrint p1, halfSpaced ". ", showPrint p2 ]

        IfThenElse p1 p2 p3 ->
            g [ txt "if ", showPrint p1, txt " then ", showPrint p2, txt " else ", showPrint p3 ]

        ArrowOver p1 ->
            styled S.span
                [ position relative ]
                []
                [ showPrint p1
                , styled S.span [ position absolute, left <| em 0.15, top <| em 0, fontSize <| em 0.5, fontWeight bold ] [] [ txt "→" ]
                ]

        Let p1 p2 p3 ->
            g [ txt "let ", showPrint p1, txt " = ", showPrint p2, txt " in ", showPrint p3 ]

        AddToCtx p1 p2 ->
            g [ showPrint p1, txt ", ", showPrint p2 ]

        SubstType p1 p2 p3 ->
            g [ txt "{", showPrint p1, txt "/", showPrint p2, txt "}", showPrint p3 ]

        TypeAppl p1 p2 ->
            g [ showPrint p1, txt " [", showPrint p2, txt "]" ]

        TypeAbs p1 p2 ->
            g [ txt "Λ", showPrint p1, halfSpaced ". ", showPrint p2 ]

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


getRuleTemplate : Rule -> RuleTemplate
getRuleTemplate rule =
    case rule of
        TTrue ->
            { tops = [], bottom = Imply Gamma (OfType (Const "true") (Const "Bool")) }

        TFalse ->
            { tops = [], bottom = Imply Gamma (OfType (Const "false") (Const "Bool")) }

        TVar ->
            { tops =
                [ ElemOf (OfType (Var "x") Sigma) Gamma
                ]
            , bottom =
                Imply Gamma
                    (OfType (Var "x") Sigma)
            }

        TVarInst ->
            { tops =
                [ ElemOf (OfType (Var "x") Sigma) Gamma
                , Subset Sigma Tau
                ]
            , bottom =
                Imply Gamma
                    (OfType (Var "x") Tau)
            }

        TAbs ->
            { tops =
                [ Imply (AddToCtx Gamma (OfType (Var "x") Tau))
                    (OfType (SubExpr "M") (Tau |> Prime))
                ]
            , bottom =
                Imply Gamma
                    (OfType (Abs (Var "x") (SubExpr "M")) (Arrow Tau (Tau |> Prime)))
            }

        TApp ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") (Arrow Sigma Tau))
                , Imply Gamma
                    (OfType (SubExpr "N") Tau)
                ]
            , bottom =
                Imply Gamma
                    (OfType (Appl (SubExpr "M") (SubExpr "N")) Tau)
            }

        TIf ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") (Const "Bool"))
                , Imply Gamma
                    (OfType (SubExpr "N") Sigma)
                , Imply Gamma
                    (OfType (SubExpr "L") Sigma)
                ]
            , bottom =
                Imply Gamma
                    (OfType (IfThenElse (SubExpr "M") (SubExpr "N") (SubExpr "L")) Sigma)
            }

        TTAbs ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") Sigma)
                , NotElem Alpha (FV Gamma)
                ]
            , bottom =
                Imply Gamma
                    (OfType (TypeAbs Alpha (SubExpr "M")) (Forall Alpha Sigma))
            }

        TTApp ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") (Forall Alpha Sigma))
                ]
            , bottom =
                Imply Gamma
                    (OfType (TypeAppl (SubExpr "M") (Sigma |> Prime)) (SubstType (Sigma |> Prime) Alpha Sigma))
            }

        TLet ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") Sigma)
                , Imply (AddToCtx Gamma (OfType (Var "x") Sigma))
                    (OfType (SubExpr "N") Tau)
                ]
            , bottom =
                Imply Gamma
                    (OfType (Let (Var "x") (SubExpr "M") (SubExpr "N")) Tau)
            }

        TLetGen ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") Sigma)
                , Imply (AddToCtx Gamma (OfType (Var "x") (Forall (ArrowOver Alpha) Tau)))
                    (OfType (SubExpr "N") (Tau |> Prime))
                ]
            , bottom =
                Imply Gamma
                    (OfType (Let (Var "x") (SubExpr "M") (SubExpr "N")) (Tau |> Prime))
            }

        TGen ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") Sigma)
                , NotElem Alpha (FV Gamma)
                ]
            , bottom =
                Imply Gamma
                    (OfType (SubExpr "M") (Forall Alpha Sigma))
            }

        TInst ->
            { tops =
                [ Imply Gamma
                    (OfType (SubExpr "M") (Sigma |> Prime))
                , Subset (Sigma |> Prime) Sigma
                ]
            , bottom =
                Imply Gamma
                    (OfType (SubExpr "M") Sigma)
            }

        NoRule ->
            { tops = [], bottom = Var "Not Applicable" }


ruleList rules =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems stretch
        , margin2 (px 15) (px 0)
        ]
        []
        (rules
            |> List.map
                (\rule ->
                    getRuleTemplate rule
                        |> (\{ tops, bottom } ->
                                styled S.div
                                    [ displayFlex, flexDirection column ]
                                    []
                                    [ styled S.div [ marginLeft <| px 10, color theme.text, marginBottom <| px 12 ] [ A.class "small-caps" ] [ S.text <| showRule rule ]
                                    , styled S.div
                                        [ displayFlex, flexDirection column, alignItems center, color theme.secondary ]
                                        []
                                        [ if List.isEmpty tops then
                                            S.div [] []

                                          else
                                            styled S.div
                                                [ displayFlex, flexDirection column, alignItems stretch ]
                                                []
                                                [ styled S.div
                                                    [ displayFlex, justifyContent center ]
                                                    []
                                                    (tops |> List.map showPrint |> List.intersperse (styled S.span [ marginRight <| em 2 ] [] []))
                                                , styled S.div [ displayFlex, flex <| auto, height <| px 5, alignItems center ] [] []
                                                , styled S.div [ marginBottom <| px -1 ] [] [ hairLine ]
                                                ]
                                        , styled S.div
                                            [ displayFlex, flexDirection column, alignItems stretch ]
                                            []
                                            [ if List.isEmpty tops then
                                                S.div [] []

                                              else
                                                styled S.div
                                                    [ displayFlex, flexDirection column ]
                                                    []
                                                    [ hairLine
                                                    , styled S.div [ displayFlex, flex <| auto, height <| px 5, alignItems center ] [] []
                                                    ]
                                            , styled S.div
                                                [ displayFlex, justifyContent center ]
                                                []
                                                [ showPrint bottom ]
                                            ]
                                        ]
                                    ]
                           )
                )
            |> List.intersperse
                (styled S.div
                    [ margin2 (px 15) (px 0) ]
                    []
                    [ styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.lightLine, flex auto, margin2 (px 0) (px 10) ] [] []
                    ]
                )
        )


hairLine =
    styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.darkLine, flex auto ] [] []
