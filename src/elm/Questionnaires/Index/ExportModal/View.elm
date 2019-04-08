module Questionnaires.Index.ExportModal.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Api.Questionnaires as QuestionnairesApi
import Common.AppState exposing (AppState)
import Common.Html exposing (fa)
import Common.View.Modal as Modal
import Common.View.Page as Page
import Html exposing (Html, a, button, div, h5, input, label, option, select, text)
import Html.Attributes exposing (checked, class, classList, href, name, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Msgs
import Questionnaires.Index.ExportModal.Models exposing (Model, Template)
import Questionnaires.Index.ExportModal.Msgs exposing (Msg(..))


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    let
        ( visible, questionnaireName, questionnaireUuid ) =
            case model.questionnaire of
                Just questionnaire ->
                    ( True, questionnaire.name, questionnaire.uuid )

                Nothing ->
                    ( False, "", "" )

        modalContent =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text <| "Export " ++ questionnaireName ]
                ]
            , div [ class "modal-body" ]
                [ Page.actionResultView (viewModalContent model.selectedFormat) model.templates ]
            , div [ class "modal-footer" ]
                (modalActions appState model questionnaireUuid)
            ]

        modalConfig =
            { modalContent = modalContent
            , visible = visible
            }
    in
    Html.map wrapMsg <|
        Modal.simple modalConfig


viewModalContent : String -> List Template -> Html Msg
viewModalContent selectedFormat templates =
    div []
        [ templateFormGroup templates
        , formatFormGroup selectedFormat
        ]


templateFormGroup : List Template -> Html Msg
templateFormGroup templates =
    div [ class "form-group" ]
        [ label [] [ text "Template" ]
        , select [ class "form-control", onInput SelectTemplate ]
            (List.map templateOption templates)
        ]


templateOption : Template -> Html msg
templateOption template =
    option [ value template.uuid ] [ text template.name ]


formatFormGroup : String -> Html Msg
formatFormGroup selectedFormat =
    div [ class "form-group" ]
        [ label [] [ text "Format" ]
        , div [ class "export-formats" ]
            (List.map (exportItem selectedFormat) exportFormats)
        ]


exportItem : String -> ( String, String, String ) -> Html Msg
exportItem selected ( icon, format, formatLabel ) =
    label [ class "export-link", classList [ ( "export-link-selected", selected == format ) ] ]
        [ input
            [ type_ "radio"
            , name "format"
            , checked (selected == format)
            , onClick (SelectFormat format)
            ]
            []
        , fa icon
        , text <| formatLabel
        ]


exportFormats : List ( String, String, String )
exportFormats =
    [ ( "file-pdf-o", "pdf", "PDF Document" )
    , ( "file-text-o", "latex", "LaTeX Document" )
    , ( "file-word-o", "docx", "MS Word Document" )
    , ( "file-code-o", "html", "HTML Document" )
    , ( "file-code-o", "json", "JSON Data" )
    , ( "file-text-o", "odt", "OpenDocument Text" )
    , ( "file-text-o", "markdown", "Markdown Document" )
    ]


modalActions appState model questionnaireUuid =
    let
        downloadLink =
            QuestionnairesApi.exportQuestionnaireUrl questionnaireUuid model.selectedFormat model.selectedTemplate appState
    in
    if ActionResult.isSuccess model.templates then
        [ a [ onClick Close, class "btn btn-primary", href downloadLink, target "_blank" ]
            [ text "Download" ]
        , button [ onClick Close, class "btn btn-secondary" ]
            [ text "Cancel" ]
        ]

    else
        [ button [ onClick Close, class "btn btn-primary" ]
            [ text "Close" ]
        ]
