module ErrorReport.View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Css exposing (..)
import ErrorReport.Message exposing (Msg(..))
import ErrorReport.Model
import ErrorReport.Utils exposing (getModalState)
import Html
import Html.Events as HtmlE
import Html.Styled as S exposing (Html, styled)


errorReportModal : ErrorReport.Model.Model -> S.Html ErrorReport.Message.Msg
errorReportModal model =
    Modal.config ErrorReportDismissMsg
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h5 [] [ Html.text "Error" ]
        |> Modal.body []
            [ styled S.div
                [ displayFlex, flex auto, flexDirection column ]
                []
                [ styled S.div [] [] [ S.text (model.text |> Maybe.withDefault "") ]
                ]
                |> S.toUnstyled
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.dark
                , Button.attrs [ HtmlE.onClick ErrorReportDismissMsg ]
                ]
                [ Html.text "OK" ]
            ]
        |> Modal.view (getModalState model)
        |> S.fromUnstyled
