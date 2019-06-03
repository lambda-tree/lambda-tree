module View.HelpSection exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import View.Theme exposing (theme)


cell : S.Html msg -> S.Html msg
cell content =
    styled S.div
        [ height <| rem 2, displayFlex, alignItems center ]
        []
        [ content ]


symbol : String -> S.Html msg
symbol text =
    styled S.div
        [ fontSize <| rem 1.2, displayFlex, alignItems center ]
        []
        [ S.text text ]


keystroke : String -> S.Html msg
keystroke text =
    styled S.kbd
        [ displayFlex, padding <| px 5 ]
        []
        [ S.text text ]


phrase : String -> S.Html msg
phrase text =
    styled S.span
        [ displayFlex, fontFamily monospace ]
        []
        [ S.text text ]


shortcuts : List (S.Html msg) -> S.Html msg
shortcuts elements =
    elements
        |> List.intersperse (styled S.span [ width <| rem 1.2, textAlign center, color theme.darkLine ] [] [ S.text "|" ])
        |> styled S.div
            [ displayFlex, alignItems center, justifyContent center ]
            []


title : String -> S.Html msg
title text =
    styled S.div [ marginBottom <| px 20, fontSize <| rem 1.1 ] [] [ S.text text ]


subsection : String -> S.Html msg -> S.Html msg
subsection titleText content =
    styled S.div
        [ displayFlex
        , flexDirection column
        , justifyContent flexStart
        , alignItems stretch
        , margin2 (px 0) (px 10)
        ]
        []
        [ title titleText
        , content
        , styled S.div
            [ margin4 (px 25) zero (px 25) zero ]
            []
            [ styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.lightLine, flex auto ] [] []
            ]
        ]


textExpansionsSubsection : S.Html msg
textExpansionsSubsection =
    subsection "Text expansions" <|
        styled S.div
            [ displayFlex ]
            []
            [ styled S.div
                [ displayFlex, flexDirection column, padding4 zero (px 20) zero (px 10) ]
                []
                [ cell <| symbol "λ"
                , cell <| symbol "Λ"
                , cell <| symbol "∀"
                , cell <| symbol "→"
                ]
            , styled S.div
                [ displayFlex
                , flexDirection column
                , padding4 zero zero zero (px 20)
                , borderStyle solid
                , borderWidth4 zero zero zero (px 1)
                , borderColor theme.lightLine
                ]
                []
                [ cell <| shortcuts [ keystroke "\\", phrase "lambda␣" ]
                , cell <| shortcuts [ keystroke "|", keystroke "^", phrase "Lambda␣" ]
                , cell <| shortcuts [ phrase "forall␣" ]
                , cell <| shortcuts [ keystroke "->" ]
                ]
            ]


howToSubsection : S.Html msg
howToSubsection =
    subsection "How to use" <|
        styled S.div
            [ displayFlex ]
            []
            [ S.span []
                [ S.text "See the section 2.3 of the "
                , S.a
                    [ A.href <|
                        "https://is.muni.cz/th"
                            ++ "/u4h13/Web_Application_for_Construction_of_Type_Derivation_Trees.pdf#page=36"
                    , A.target "_blank"
                    ]
                    [ S.text "thesis" ]
                ]
            ]


contributionSubsection : S.Html msg
contributionSubsection =
    subsection "How to contribute" <|
        styled S.div
            [ displayFlex ]
            []
            [ S.span []
                [ S.text "Feel free to submit a pull request and report issues on "
                , S.a
                    [ A.href "https://github.com/lambda-tree/lambda-tree"
                    , A.target "_blank"
                    ]
                    [ S.text "GitHub" ]
                ]
            ]


helpSection : S.Html msg
helpSection =
    styled S.div
        [ displayFlex
        , flex auto
        , flexDirection column
        , justifyContent flexStart
        , alignItems stretch
        , margin2 (px 15) (px 0)
        ]
        []
        [ textExpansionsSubsection, howToSubsection, contributionSubsection ]
