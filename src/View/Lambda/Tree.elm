module View.Lambda.Tree exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Events as E
import Message exposing (Msg(..))
import Utils.Tree exposing (Tree(..))
import View.Lambda.ProofCell exposing (proofCell)
import View.Lambda.RuleSelector exposing (ruleSelector)
import View.Theme exposing (theme)
import ViewModel exposing (TreeViewData, getTreeViewData)


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
                                ]

                          else
                            S.div [] []
                        , styled S.div
                            [ displayFlex, margin2 (rem 0) (rem -1.5), alignItems flexEnd ]
                            []
                            (children
                                |> List.indexedMap (\i t1 -> drawTreeP t1 (path ++ [ i ]))
                            )
                        , styled S.div
                            [ displayFlex, flexDirection column, alignItems stretch ]
                            []
                            [ if List.isEmpty children then
                                S.div [] []

                              else
                                hairline path content.rule content.result
                            , proofCell content (TextChangedMsg path)
                            ]
                        ]
    in
    drawTreeP (getTreeViewData (Debug.log "tree" tree)) []


hairline path rule text =
    styled S.div
        [ displayFlex, justifyContent stretch, alignItems center ]
        []
        [ styled S.div [ height <| px 1, minHeight <| px 1, backgroundColor <| theme.darkLine, flex auto ] [] []
        , styled S.button [ theme.font, margin2 (rem 0) (rem 0.5) ] [ E.onClick <| RemoveMsg path ] [ S.text <| "Remove" ]
        , styled S.div [] [] [ S.text <| Debug.toString rule ++ " | " ++ text ]
        ]
