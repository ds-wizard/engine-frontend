module Wizard.Projects.Create.TemplateCreate.View exposing (..)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Html exposing (..)
import Html.Events exposing (onSubmit)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Locale exposing (l, lg)
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


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Create.TemplateCreate.View"


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
            Routes.projectsIndex
            (ActionResult.ButtonConfig (l_ "form.save" appState) model.savingQuestionnaire (FormMsg Form.Submit) False)
        ]


formView : AppState -> Model -> Maybe QuestionnaireDetail -> Html Msg
formView appState model mbQuestionnaire =
    let
        cfg =
            { viewItem = TypeHintItem.questionnaireSuggestion
            , wrapMsg = QuestionnaireTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = True
            }

        typeHintInput =
            TypeHintInput.view appState cfg model.questionnaireTypeHintInputModel

        parentInput =
            case mbQuestionnaire of
                Just questionnaire ->
                    let
                        value =
                            TypeHintItem.questionnaireSuggestion questionnaire
                    in
                    FormGroup.plainGroup value (lg "questionnaire.templateBadge" appState)

                Nothing ->
                    FormGroup.formGroupCustom typeHintInput appState model.form "questionnaireUuid" (lg "questionnaire.templateBadge" appState)
    in
    div []
        [ Html.map FormMsg <| FormGroup.input appState model.form "name" <| lg "questionnaire.name" appState
        , parentInput
        ]
