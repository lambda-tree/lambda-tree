module RuleTree.View.Tree exposing (..)

import Bootstrap.Button as Button
import Css exposing (..)
import Html.Attributes as HtmlA
import Html.Styled as S exposing (styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import List.Extra
import RuleTree.Message exposing (..)
import RuleTree.Model exposing (Rule(..), RuleTree)
import RuleTree.View.ProofCell exposing (proofCell)
import RuleTree.ViewModel exposing (TreeViewData, getTreeViewData)
import Utils.Tree exposing (Tree(..))
import View.Lambda.RuleSelector exposing (rulePlus, ruleSelector)
import View.Theme exposing (theme)


sp =
    styled S.div
        [ flex auto
        , minWidth <| rem 1.5
        ]
        []
        []


hairlineSp =
    styled S.div
        [ displayFlex, flexDirection column, alignItems stretch, justifyContent stretch, flex auto ]
        []
        [ sp
        , styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.darkLine ] [] []
        ]


drawTree : RuleTree -> S.Html Msg
drawTree tree =
    let
        drawTreeP : TreeViewData -> List Int -> Int -> S.Html Msg
        drawTreeP t path childCount =
            case t of
                Node content children ->
                    styled S.div
                        [ displayFlex, flexDirection column, alignItems stretch ]
                        []
                        [ if List.length children == 0 then
                            styled S.div
                                [ displayFlex, flexDirection column, alignItems center, justifyContent center, flex auto ]
                                []
                                [ rulePlus <| RuleSelectedMsg path TTAbs
                                , styled S.div [ width <| px 1, minWidth <| px 1, backgroundColor <| theme.darkLine, height <| px 10 ] [] []
                                ]

                          else
                            S.div [] []
                        , --                           Draw children in div
                          styled S.div
                            [ displayFlex
                            , alignItems flexEnd
                            , marginBottom <| px -11
                            ]
                            []
                            (children
                                |> List.indexedMap (\i t1 -> drawTreeP t1 (path ++ [ i ]) (List.length children))
                            )
                        , styled S.div
                            [ displayFlex
                            , alignItems stretch
                            , justifyContent stretch

                            --                            , backgroundColor <| rgba 0 0 255 0.2
                            ]
                            []
                            [ -- Draw hairline below between rules on left
                              case List.Extra.last path of
                                Just 0 ->
                                    sp

                                Nothing ->
                                    sp

                                _ ->
                                    hairlineSp
                            , styled S.div
                                [ displayFlex
                                , flexDirection column
                                , alignItems stretch
                                ]
                                []
                                [ -- hairline above rule
                                  ruleLine path content.rule content.result
                                , proofCell content (TextChangedMsg path)
                                , styled S.div [ height <| px 10 ] [] []
                                , if List.isEmpty path then
                                    S.div [] []

                                  else
                                    hairLine
                                ]

                            -- Draw hairline below between rules on right
                            , case List.Extra.last path of
                                Just x ->
                                    if x == childCount - 1 then
                                        sp

                                    else
                                        hairlineSp

                                _ ->
                                    sp
                            ]

                        -- Cells and hairline
                        ]
    in
    drawTreeP (getTreeViewData (Debug.log "tree" tree)) [] 1


ruleLine path rule text =
    styled S.div
        [ displayFlex, justifyContent stretch, alignItems center, height <| px 18, minHeight <| px 20 ]
        []
        [ hairLine
        , styled S.span
            [ displayFlex, backgroundColor theme.background, alignItems center ]
            []
            [ styled S.span [ width <| px 10, backgroundColor theme.background ] [] []
            , resultBut (RemoveMsg path)
            , styled S.span
                [ width <| px 8, height <| pct 100 ]
                []
                []
            , Button.button
                [ Button.small, Button.outlineDark, Button.onClick (RuleSelectedMsg path TIf), Button.attrs [ HtmlA.style "borderWidth" "0", HtmlA.style "outline" "none" ] ]
                [ S.text (Debug.toString rule) |> S.toUnstyled ]
                |> S.fromUnstyled
            , styled
                S.span
                [ width <| px 10 ]
                []
                []
            ]
        , hairLine
        ]


hairLine =
    styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.darkLine, flex auto ] [] []


resultBut msg =
    styled S.button
        [ backgroundColor <| rgb 230 55 86
        , color <| rgb 234 230 220
        , padding <| px 2
        , borderRadius <| px 10
        , width <| px 18
        , height <| px 18
        , fontFamilies [ "cmunrm" ]
        , fontSize <| rem 0.7
        , outline zero
        , borderWidth zero
        , cursor pointer
        , hover
            [ boxShadow4 (px 0) (px 0) (px 5) (rgba 0 0 0 0.3)
            ]
        , active
            [ opacity <| num 0.7
            , outline zero
            ]
        , focus [ outline zero ]
        ]
        [ E.onClick msg ]
        [ S.i [ A.class "fas fa-exclamation" ] [] ]
