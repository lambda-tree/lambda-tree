module View.Layout exposing (..)

import Css exposing (..)
import Html.Styled as S exposing (Html, styled)
import Html.Styled.Events as E
import Message exposing (Msg(..))
import Substitutor.Message
import View.Lambda.ExpressionInput exposing (lambdaExprInput)
import View.Lambda.ExpressionText exposing (lambdaExprText)
import View.Lambda.Tree exposing (drawTree)
import View.Theme exposing (theme)


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
        [ leftColumn model.zoomLevel
            [ treeContainer
                [ drawTree model.tree
                ]
            ]
        , rightColumn
            [ scroller
                [ styled S.button [ margin <| rem 0.5 ] [ E.onClick ClickedMsg ] [ S.text "Click" ]
                , styled S.button [ margin <| rem 0.5 ] [ E.onClick ZoomIn ] [ S.text "Zoom In" ]
                , styled S.button [ margin <| rem 0.5 ] [ E.onClick ZoomOut ] [ S.text "Zoom Out" ]
                , styled S.div
                    [ displayFlex, flexDirection column, flex <| int 1 ]
                    []
                    [ S.text "Substitute free variable"
                    , styled S.div
                        [ flex <| int 1 ]
                        []
                        [ styled S.div
                            [ displayFlex
                            , flex <| int 1
                            ]
                            []
                            [ lambdaExprInput False model.substitution.ty (Substitutor.Message.TyChanged >> SubstitutionMsg) ]
                        , lambdaExprText "/"
                        , styled S.div
                            [ displayFlex
                            , flex <| int 1
                            ]
                            []
                            [ lambdaExprInput False model.substitution.var (Substitutor.Message.VarChanged >> SubstitutionMsg) ]
                        , styled S.button [ margin <| rem 0.5 ] [ E.onClick DoSubstitutionMsg ] [ S.text "Substitute" ]
                        ]
                    ]
                ]
            ]
        ]


leftColumn zoomLevel children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 8
        , alignItems flexStart
        , justifyContent flexStart
        , overflow auto
        , whiteSpace noWrap
        , property "zoom" <| String.fromFloat zoomLevel ++ "%"
        ]
        []
        children


rightColumn children =
    styled S.div
        [ displayFlex
        , flexDirection column
        , flex <| int 2
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
