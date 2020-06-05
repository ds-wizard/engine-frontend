module Wizard.Documents.Create.View exposing (..)

import ActionResult exposing (ActionResult(..))
import Form
import Html exposing (..)
import Html.Attributes exposing (class)
import List.Extra as List
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.Pagination.PaginationQueryString as PaginationQueryString
import Wizard.Common.Questionnaire.Views.SummaryReport exposing (viewIndications)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Documents.Common.Template exposing (Template)
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
                (Routes.DocumentsRoute <| IndexRoute Nothing PaginationQueryString.empty)
                (ActionResult.ButtonConfig (l_ "form.create" appState) model.savingDocument (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Model -> List Questionnaire -> Html Form.Msg
formView appState model questionnaires =
    let
        questionnaireOptions =
            ( "", "--" ) :: (List.map (\q -> ( q.uuid, q.name )) <| List.sortBy (String.toLower << .name) questionnaires)

        selectedQuestionnaire =
            (Form.getFieldAsString "questionnaireUuid" model.form).value
                |> Maybe.andThen (\qUuid -> List.find (\q -> q.uuid == qUuid) questionnaires)

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

        questionnaireDetail =
            case selectedQuestionnaire of
                Just questionnaire ->
                    div [ class "form-group" ]
                        [ viewIndications appState questionnaire.report.indications
                        ]

                Nothing ->
                    emptyNode

        templatesInput =
            case model.templates of
                Success templates ->
                    let
                        createTemplateOption { uuid, name } =
                            let
                                visibleName =
                                    if appState.config.template.recommendedTemplateUuid == Just uuid then
                                        name ++ " (" ++ l_ "template.recommended" appState ++ ")"

                                    else
                                        name
                            in
                            ( uuid, visibleName )

                        templateOptions =
                            ( "", "--" ) :: (List.map createTemplateOption <| List.sortBy (String.toLower << .name) templates)
                    in
                    FormGroup.select appState templateOptions model.form "templateUuid" <| lg "template" appState

                _ ->
                    Flash.actionResult appState model.templates

        mbSelectedTemplateUuid =
            (Form.getFieldAsString "templateUuid" model.form).value

        mbSelectedTemplate =
            model.templates
                |> ActionResult.map (List.find (.uuid >> Just >> (==) mbSelectedTemplateUuid))
                |> ActionResult.withDefault Nothing

        formatInput =
            case mbSelectedTemplate of
                Just selectedTemplate ->
                    FormGroup.formatRadioGroup appState selectedTemplate.formats model.form "formatUuid" <| lg "template.format" appState

                _ ->
                    emptyNode
    in
    div []
        [ FormGroup.input appState model.form "name" <| lg "document.name" appState
        , questionnaireInput
        , questionnaireDetail
        , templatesInput
        , formatInput
        ]
