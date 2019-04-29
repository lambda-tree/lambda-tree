module RuleTree.View.Tree exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import List.Extra
import RuleTree.Message exposing (..)
import RuleTree.Model exposing (Rule(..), RuleTree)
import RuleTree.View.ProofCell exposing (proofCell)
import RuleTree.ViewModel exposing (TreeViewData, getTreeViewData)
import Utils.Tree exposing (Tree(..))
import View.Lambda.RuleSelector exposing (newRuleDDButton, ruleDropDown, selectedRuleDDButton)
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
                                [ ruleDropDown newRuleDDButton path content.dropdown
                                , styled S.div [ width <| px 1, minWidth <| px 1, backgroundColor <| theme.darkLine, height <| px 10 ] [] []
                                ]

                          else
                            S.div [] []
                        , -- Draw children in div
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
                                [ -- hairline and rule selector above rule
                                  ruleLine (content.rule /= NoRule) path content.rule content.result content.dropdown
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


ruleLine showDrop path rule text dropdownState =
    styled S.div
        [ displayFlex, justifyContent stretch, alignItems center, height <| px 18, minHeight <| px 20 ]
        []
        [ hairLine
        , if showDrop then
            styled S.span
                [ displayFlex, backgroundColor theme.background, alignItems center ]
                []
                [ styled S.span [ width <| px 10, backgroundColor theme.background ] [] []
                , resultBut (RemoveMsg path)
                , styled S.span
                    [ width <| px 2, height <| pct 100 ]
                    []
                    []
                , ruleDropDown (selectedRuleDDButton rule) path dropdownState
                , styled
                    S.span
                    [ width <| px 10 ]
                    []
                    []
                ]

          else
            S.span [] []
        , hairLine
        ]


hairLine =
    styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.darkLine, flex auto ] [] []


resultBut msg =
    styled S.button
        [ --        backgroundColor <| rgb 230 55 86
          --        , color <| rgb 234 230 220
          backgroundColor <| theme.clear
        , color <| rgb 230 55 86
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
