module View.GlobalCss exposing (..)

import Css exposing (..)
import Css.Global exposing (body, global, html, selector)
import Html.Styled as S
import View.Theme exposing (theme)


style : S.Html msg
style =
    global
        [ html [ fontSize <| px 16 ]
        , body [ backgroundColor theme.background, margin <| px 0, lineHeight <| num 1, theme.font ]
        , selector "@font-face"
            [ property "src" "url(/fonts/cmunrm.ttf)"
            , fontStyle normal
            , theme.cmFont
            ]
        , selector "@font-face"
            [ property "src" "url(/fonts/cmunbx.ttf)"
            , fontStyle normal
            , fontWeight bold
            , theme.cmFont
            ]
        , selector "@font-face"
            [ property "src" "url(/fonts/cmunti.ttf)"
            , fontStyle italic
            , theme.cmFont
            ]
        , selector "@font-face"
            [ property "src" "url(/fonts/KaTeX_Main-Regular.ttf)"
            , fontStyle normal
            , theme.ktFont
            ]
        ]
