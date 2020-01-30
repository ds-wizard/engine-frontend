module Wizard.Documents.Create.View exposing (..)

import ActionResult exposing (ActionResult(..))
import Form
import Html exposing (..)
import Shared.Locale exposing (l, lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (emptyNode, faSet)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Documents.Create.Models exposing (Model)
import Wizard.Documents.Create.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Documents.Create.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (content appState model) model.questionnaires


content : AppState -> Model -> List Questionnaire -> Html Msg
content appState model questionnaires =
    div [ detailClass "Documents__Create" ]
        [ Page.header (l_ "header.title" appState) []
        , div []
            [ FormResult.view appState model.savingDocument
            , Html.map FormMsg <| formView appState model questionnaires
            , FormActions.view appState
                (Routes.DocumentsRoute <| IndexRoute Nothing)
                (ActionResult.ButtonConfig (l_ "form.create" appState) model.savingDocument (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Model -> List Questionnaire -> Html Form.Msg
formView appState model questionnaires =
    let
        questionnaireOptions =
            ( "", "--" ) :: (List.map (\q -> ( q.uuid, q.name )) <| List.sortBy (String.toLower << .name) questionnaires)

        questionnaireInput =
            case model.selectedQuestionnaire of
                Just questionnaireUuid ->
                    let
                        questionnaireName =
                            List.filter (\q -> q.uuid == questionnaireUuid) questionnaires
                                |> List.head
                                |> Maybe.map .name
                                |> Maybe.withDefault ""
                    in
                    FormGroup.textView questionnaireName <| lg "questionnaire" appState

                Nothing ->
                    FormGroup.select appState questionnaireOptions model.form "questionnaireUuid" <| lg "questionnaire" appState

        templatesInput =
            case model.templates of
                Success templates ->
                    let
                        templateOptions =
                            ( "", "--" ) :: (List.map (\t -> ( t.uuid, t.name )) <| List.sortBy (String.toLower << .name) templates)
                    in
                    FormGroup.select appState templateOptions model.form "templateUuid" <| lg "template" appState

                _ ->
                    Flash.actionResult appState model.templates

        formatInput =
            case model.templates of
                Success _ ->
                    FormGroup.formatRadioGroup appState (exportFormats appState) model.form "format" <| lg "template.format" appState

                _ ->
                    emptyNode
    in
    div []
        [ FormGroup.input appState model.form "name" <| lg "document.name" appState
        , questionnaireInput
        , templatesInput
        , formatInput
        ]


exportFormats : AppState -> List ( Html msg, String, String )
exportFormats appState =
    [ ( faSet "format.pdf" appState, "pdf", lg "template.format.pdf" appState )
    , ( faSet "format.text" appState, "latex", lg "template.format.latex" appState )
    , ( faSet "format.word" appState, "docx", lg "template.format.docx" appState )
    , ( faSet "format.code" appState, "html", lg "template.format.html" appState )
    , ( faSet "format.code" appState, "json", lg "template.format.json" appState )
    , ( faSet "format.text" appState, "odt", lg "template.format.odt" appState )
    , ( faSet "format.text" appState, "markdown", lg "template.format.markdown" appState )
    ]
