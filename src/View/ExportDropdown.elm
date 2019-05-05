module View.ExportDropdown exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Html.Styled as S
import Message exposing (Msg(..))


exportDropdown : Dropdown.State -> S.Html Message.Msg
exportDropdown dropdownState =
    Dropdown.dropdown
        dropdownState
        { options = []
        , toggleMsg = ExportDropdownMsg
        , toggleButton =
            Dropdown.toggle
                [ Button.small
                , Button.dark
                ]
                [ Html.text <| "Export" ]
        , items =
            [ Dropdown.buttonItem [ HtmlE.onClick <| ExportLaTexMsg, HtmlA.disabled False ] [ Html.text "LaTeX" ]
            , Dropdown.buttonItem [ HtmlE.onClick <| ExportJsonMsg, HtmlA.disabled False ] [ Html.text "JSON" ]
            ]
        }
        |> S.fromUnstyled
