module RuleTree.View.Tree exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Lambda.Rule exposing (Rule(..), isTerminalRule, rulesForTypeSystem)
import List.Extra
import RuleTree.Message exposing (..)
import RuleTree.Model exposing (RuleTree)
import RuleTree.View.ProofCell exposing (proofCell)
import RuleTree.View.RuleDropdown exposing (newRuleDDButton, ruleDropdown, selectedRuleDDButton)
import RuleTree.ViewModel exposing (TreeViewData, getTreeViewData)
import Settings.Model
import Settings.Utils exposing (getTypeSystem)
import Utils.Tree exposing (Tree(..))
import View.Theme exposing (theme)


treeView : Settings.Model.Model -> RuleTree -> S.Html Msg
treeView settings tree =
    let
        typeSystem =
            getTypeSystem settings

        rules =
            rulesForTypeSystem typeSystem

        drawTreeP : TreeViewData -> List Int -> Int -> Rule -> S.Html Msg
        drawTreeP (Node content children) path childCount rule =
            styled S.div
                [ displayFlex, flexDirection column, alignItems stretch ]
                []
                [ -- Draw rule adding button
                  if List.length children == 0 && (not << isTerminalRule) content.rule && (not << isTerminalRule) rule then
                    styled S.div
                        [ displayFlex, flexDirection column, alignItems center, justifyContent center, flex auto ]
                        []
                        [ ruleDropdown content.dropdown { button = newRuleDDButton, path = path, rules = rules }
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
                        |> List.indexedMap (\i t1 -> drawTreeP t1 (path ++ [ i ]) (List.length children) content.rule)
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
                            ruleFiller

                        Nothing ->
                            ruleFiller

                        _ ->
                            hairlineRuleFiller
                    , styled S.div
                        [ displayFlex
                        , flexDirection column
                        , alignItems stretch
                        ]
                        []
                        [ -- hairline and rule selector above rule
                          ruleLine content.dropdown { showDropdown = content.rule /= NoRule, path = path, selectedRule = content.rule, rules = rules, result = content.result }
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
                                ruleFiller

                            else
                                hairlineRuleFiller

                        _ ->
                            ruleFiller
                    ]

                -- Cells and hairline
                ]
    in
    drawTreeP (getTreeViewData (Debug.log "tree" tree)) [] 1 NoRule


ruleLine dropdownState { showDropdown, path, selectedRule, rules, result } =
    styled S.div
        [ displayFlex, justifyContent stretch, alignItems center, height <| px 18, minHeight <| px 20 ]
        []
        [ hairLine
        , if showDropdown then
            styled S.span
                [ displayFlex, backgroundColor theme.background, alignItems center ]
                []
                [ styled S.span [ width <| px 10, backgroundColor theme.background ] [] []
                , resultBut (RemoveMsg path)
                , S.text result
                , styled S.span
                    [ width <| px 2, height <| pct 100 ]
                    []
                    []
                , ruleDropdown dropdownState { button = selectedRuleDDButton selectedRule, path = path, rules = rules }
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


ruleFiller =
    styled S.div
        [ flex auto
        , minWidth <| rem 1.5
        ]
        []
        []


hairlineRuleFiller =
    styled S.div
        [ displayFlex, flexDirection column, alignItems stretch, justifyContent stretch, flex auto ]
        []
        [ ruleFiller
        , styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.darkLine ] [] []
        ]
