module Wizard.Questionnaires.Index.ExportModal.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, a, button, div, h5, input, label, option, select, text)
import Html.Attributes exposing (checked, class, classList, href, name, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Shared.Locale exposing (l, lg, lgx, lx)
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (faSet)
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Questionnaires.Common.Template exposing (Template)
import Wizard.Questionnaires.Index.ExportModal.Models exposing (Model)
import Wizard.Questionnaires.Index.ExportModal.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Questionnaires.Index.ExportModal.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Questionnaires.Index.ExportModal.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( visible, questionnaireName, questionnaireUuid ) =
            case model.questionnaire of
                Just questionnaire ->
                    ( True, questionnaire.name, questionnaire.uuid )

                Nothing ->
                    ( False, "", "" )

        modalContent =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text <| l_ "title" appState ++ " " ++ questionnaireName ]
                ]
            , div [ class "modal-body" ]
                [ Page.actionResultView appState (viewModalContent appState model.selectedFormat) model.templates ]
            , div [ class "modal-footer" ]
                (modalActions appState model questionnaireUuid)
            ]

        modalConfig =
            { modalContent = modalContent
            , visible = visible
            }
    in
    Modal.simple modalConfig


viewModalContent : AppState -> String -> List Template -> Html Msg
viewModalContent appState selectedFormat templates =
    div []
        [ templateFormGroup appState templates
        , formatFormGroup appState selectedFormat
        ]


templateFormGroup : AppState -> List Template -> Html Msg
templateFormGroup appState templates =
    div [ class "form-group" ]
        [ label [] [ lgx "template" appState ]
        , select [ class "form-control", onInput SelectTemplate ]
            (List.map templateOption templates)
        ]


templateOption : Template -> Html msg
templateOption template =
    option [ value template.uuid ] [ text template.name ]


formatFormGroup : AppState -> String -> Html Msg
formatFormGroup appState selectedFormat =
    div [ class "form-group" ]
        [ label [] [ lgx "template.format" appState ]
        , div [ class "export-formats" ]
            (List.map (exportItem selectedFormat) (exportFormats appState))
        ]


exportItem : String -> ( Html Msg, String, String ) -> Html Msg
exportItem selected ( icon, format, formatLabel ) =
    label [ class "export-link", classList [ ( "export-link-selected", selected == format ) ] ]
        [ input
            [ type_ "radio"
            , name "format"
            , checked (selected == format)
            , onClick (SelectFormat format)
            ]
            []
        , icon
        , text <| formatLabel
        ]


exportFormats : AppState -> List ( Html Msg, String, String )
exportFormats appState =
    [ ( faSet "format.pdf" appState, "pdf", lg "template.format.pdf" appState )
    , ( faSet "format.text" appState, "latex", lg "template.format.latex" appState )
    , ( faSet "format.word" appState, "docx", lg "template.format.docx" appState )
    , ( faSet "format.code" appState, "html", lg "template.format.html" appState )
    , ( faSet "format.code" appState, "json", lg "template.format.json" appState )
    , ( faSet "format.text" appState, "odt", lg "template.format.odt" appState )
    , ( faSet "format.text" appState, "markdown", lg "template.format.markdown" appState )
    ]


modalActions : AppState -> Model -> String -> List (Html Msg)
modalActions appState model questionnaireUuid =
    let
        downloadLink =
            QuestionnairesApi.exportQuestionnaireUrl questionnaireUuid model.selectedFormat model.selectedTemplate appState
    in
    if ActionResult.isSuccess model.templates then
        [ a [ onClick Close, class "btn btn-primary", href downloadLink, target "_blank" ]
            [ lx_ "action.download" appState ]
        , button [ onClick Close, class "btn btn-secondary" ]
            [ lx_ "action.cancel" appState ]
        ]

    else
        [ button [ onClick Close, class "btn btn-primary" ]
            [ lx_ "action.close" appState ]
        ]
