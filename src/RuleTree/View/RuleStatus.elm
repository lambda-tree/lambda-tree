module RuleTree.View.RuleStatus exposing (..)

import Bootstrap.Popover as Popover
import Css exposing (..)
import Html
import Html.Events as HtmlE
import Html.Styled as S exposing (styled)
import Html.Styled.Attributes as A
import Result.Extra
import RuleTree.Message exposing (Msg(..))
import View.Theme exposing (theme)


ruleStatus { popover, result, path } =
    Popover.config
        (styled S.button
            [ displayFlex
            , backgroundColor <| theme.clear
            , color <|
                if Result.Extra.isErr result then
                    theme.errorText

                else
                    theme.successText
            , padding2 (px 4) (px 0)
            , outline zero
            , borderWidth zero
            , cursor pointer
            , hover
                [ textShadow4 (px 0) (px 0) (px 5) (rgba 255 255 255 1)
                , opacity <| num 0.8
                ]
            , active
                [ opacity <| num 0.8
                , outline zero
                ]
            , focus [ outline zero ]
            ]
            (Popover.onHover popover (RuleStatusPopoverMsg path) |> List.map A.fromUnstyled)
            [ styled S.i
                [ fontSize <| rem 1.125 ]
                [ A.class "material-icons" ]
                [ S.text <|
                    if Result.Extra.isErr result then
                        "error"

                    else
                        "check"
                ]
            ]
            |> S.toUnstyled
        )
        |> Popover.left
        |> Popover.content []
            [ result
                |> Result.map (\_ -> "Rule is applied correctly")
                |> Result.Extra.merge
                |> Html.text
            ]
        |> Popover.view popover
        |> S.fromUnstyled
