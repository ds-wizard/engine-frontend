module Wizard.Projects.Create.TemplateCreate.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Form
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Events exposing (onSubmit)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as ActionResult
import Wizard.Projects.Create.TemplateCreate.Models exposing (Model)
import Wizard.Projects.Create.TemplateCreate.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        actionResult =
            if ActionResult.isUnset model.templateQuestionnaire then
                Success Nothing

            else
                ActionResult.map Just model.templateQuestionnaire
    in
    ActionResult.actionResultView appState (viewContent appState model) actionResult


viewContent : AppState -> Model -> Maybe QuestionnaireDetail -> Html Msg
viewContent appState model mbQuestionnaire =
    div [ onSubmit (FormMsg Form.Submit) ]
        [ FormResult.view appState model.savingQuestionnaire
        , formView appState model mbQuestionnaire
        , FormActions.view appState
            (Routes.projectsIndex appState)
            (ActionResult.ButtonConfig (gettext "Create" appState.locale) model.savingQuestionnaire (FormMsg Form.Submit) False)
        ]


formView : AppState -> Model -> Maybe QuestionnaireDetail -> Html Msg
formView appState model mbQuestionnaire =
    let
        parentInput =
            case mbQuestionnaire of
                Just questionnaire ->
                    let
                        value =
                            TypeHintItem.questionnaireSuggestion questionnaire
                    in
                    FormGroup.plainGroup value (gettext "Project Template" appState.locale)

                Nothing ->
                    let
                        cfg =
                            { viewItem = TypeHintItem.questionnaireSuggestion
                            , wrapMsg = QuestionnaireTypeHintInputMsg
                            , nothingSelectedItem = text "--"
                            , clearEnabled = True
                            }

                        typeHintInput =
                            TypeHintInput.view appState cfg model.questionnaireTypeHintInputModel
                    in
                    FormGroup.formGroupCustom typeHintInput appState model.form "questionnaireUuid" (gettext "Project Template" appState.locale)
    in
    div []
        [ Html.map FormMsg <| FormGroup.input appState model.form "name" <| gettext "Name" appState.locale
        , parentInput
        ]
