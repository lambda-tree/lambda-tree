module View.Switch exposing (..)

import Css exposing (..)
import Css.Transitions
import Html.Styled as S exposing (styled)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import View.Theme as Theme


switch isChecked onChange =
    styled S.label
        [ position relative
        , display inlineBlock
        , width <| px 38
        , height <| px 22
        ]
        []
        [ styled S.input
            [ opacity <| num 0
            , width <| px 0
            , height <| px 0
            ]
            [ A.type_ "checkbox", A.checked isChecked, E.onCheck onChange ]
            []
        , styled S.span
            ([ position absolute
             , cursor pointer
             , top <| px 0
             , left <| px 0
             , right <| px 0
             , bottom <| px 0
             , backgroundColor <| rgb 2 2 2
             , borderRadius <| px 34
             , Css.Transitions.transition
                [ Css.Transitions.backgroundColor 200
                ]
             , before
                ([ position absolute
                 , property "content" "\"\""
                 , height <| px 18
                 , width <| px 18
                 , left <| px 2
                 , bottom <| px 2
                 , top <| px 2
                 , backgroundColor <| rgb 255 255 255
                 , boxShadow4 (px 0) (px 0) (px 7) (rgba 0 0 0 0.2)
                 , Css.Transitions.transition
                    [ Css.Transitions.transform 200
                    ]
                 , borderRadius <| pct 50
                 ]
                    ++ (if isChecked then
                            [ transform <| translateX <| px 16 ]

                        else
                            []
                       )
                )
             ]
                ++ (if isChecked then
                        [ backgroundColor <| Theme.theme.active ]

                    else
                        []
                   )
            )
            []
            []
        ]
