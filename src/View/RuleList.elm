module View.RuleList exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Attributes as A
import Lambda.Rule exposing (Rule(..), showRule)
import Lambda.Show.Html exposing (show)
import Lambda.Show.RuleTemplate exposing (ruleTemplateForRule)
import View.Theme exposing (theme)


ruleList : List Rule -> S.Html msg
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
                    ruleTemplateForRule rule
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
                                                    (tops |> List.map show |> List.intersperse (styled S.span [ marginRight <| em 2 ] [] []))
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
                                                [ show bottom ]
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
