module View.Lambda.Tree exposing (..)

import Html.Styled.Events as E
import Html.Styled as S exposing (Html, styled)
import Css exposing (..)
import Message exposing (Msg(..))
import Model exposing (ModelContent, TextKind(..), Tree(..))
import View.Lambda.ProofCell exposing (proofCell)
import View.Lambda.RuleSelector exposing (ruleSelector)
import View.Theme exposing (theme)
import ViewModel exposing (getTreeViewData, TreeViewData)


drawTree tree =
    let
        drawTreeP : TreeViewData -> List Int -> S.Html Msg
        drawTreeP t path =
            case t of
                Node content children ->
                    styled S.div
                        [ displayFlex, flexDirection column, alignItems center, padding2 (rem 0) (rem 1.5) ]
                        []
                        [ if List.length children == 0 then
                            S.div []
                                [ ruleSelector <| RuleSelectedMsg path
                                , S.button [ E.onClick <| AddMsg path ] [ S.text "Add" ]
                                , S.button [ E.onClick <| RemoveMsg path ] [ S.text "Remove" ]
                                ]
                          else
                            S.div [] []
                        , styled S.div
                            [ displayFlex, margin2 (rem 0) (rem -1.5), alignItems flexEnd ]
                            []
                            (children
                                |> List.indexedMap (\i t1 -> drawTreeP t1 (path ++ [ i ]))
                            )
                        , if List.length children <= 1 then
                            S.div [] []
                          else
                            hairline content.rule content.result
                        , styled S.div
                            [ displayFlex, flexDirection column, alignItems stretch ]
                            []
                            [ if List.isEmpty children then
                                S.div [] []
                              else
                                hairline content.rule content.result
                            , proofCell content (TextChangedMsg path)
                            ]
                        ]
    in
        drawTreeP (getTreeViewData tree) []


hairline rule text =
    styled S.div
        [ displayFlex, justifyContent stretch, alignItems center ]
        []
        [ styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.darkLine, flex auto ] [] []
        , styled S.div [ margin2 (rem 0) (rem 1) ] [] [ S.text <| Debug.toString rule ++ " | " ++ text ]
        ]
