module Wizard.Documents.Create.View exposing (..)

import ActionResult exposing (ActionResult(..))
import Form
import Html exposing (..)
import Html.Attributes exposing (class)
import List.Extra as List
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.Template exposing (Template)
import Shared.Data.Template.TemplateState as TemplateState
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire.SummaryReport exposing (viewIndications)
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
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Documents.Create.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (content appState model) model.questionnaire


content : AppState -> Model -> QuestionnaireDetail -> Html Msg
content appState model questionnaire =
    div [ detailClass "Documents__Create" ]
        [ Page.header (l_ "header.title" appState) []
        , div []
            [ FormResult.view appState model.savingDocument
            , Html.map FormMsg <| formView appState model questionnaire
            , FormActions.view appState
                (Routes.DocumentsRoute <| IndexRoute Nothing PaginationQueryString.empty)
                (ActionResult.ButtonConfig (l_ "form.create" appState) model.savingDocument (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Model -> QuestionnaireDetail -> Html Form.Msg
formView appState model questionnaire =
    let
        questionnaireInput =
            FormGroup.textView questionnaire.name <| lg "questionnaire" appState

        questionnaireDetail =
            div [ class "form-group" ]
                [ viewIndications appState questionnaire.report.indications
                ]

        templatesInput =
            case model.templates of
                Success templates ->
                    let
                        createTemplateOption { id, name, state } =
                            let
                                visibleName =
                                    if appState.config.template.recommendedTemplateId == Just id then
                                        name ++ " (" ++ l_ "template.recommended" appState ++ ")"

                                    else
                                        name
                            in
                            ( id, visibleName, TemplateState.isUnsupported state )

                        templateOptions =
                            ( "", "--", False ) :: (List.map createTemplateOption <| List.sortBy (String.toLower << .name) templates)
                    in
                    FormGroup.selectWithDisabled appState templateOptions model.form "templateId" <| lg "template" appState

                _ ->
                    Flash.actionResult appState model.templates

        mbSelectedTemplateId =
            (Form.getFieldAsString "templateId" model.form).value

        mbSelectedTemplate =
            model.templates
                |> ActionResult.map (List.find (.id >> Just >> (==) mbSelectedTemplateId))
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
