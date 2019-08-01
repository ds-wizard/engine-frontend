module Questionnaires.Edit.View exposing (formView, questionnaireView, view)

import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Html exposing (emptyNode)
import Common.Html.Attribute exposing (detailClass)
import Common.View.ActionButton as ActionButton
import Common.View.FormActions as FormActions
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (Html, div)
import Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility)
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Questionnaires.Common.QuestionnaireEditForm exposing (QuestionnaireEditForm)
import Questionnaires.Edit.Models exposing (Model)
import Questionnaires.Edit.Msgs exposing (Msg(..))
import Questionnaires.Routing
import Routing exposing (Route(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (questionnaireView appState model) model.questionnaire


questionnaireView : AppState -> Model -> QuestionnaireDetail -> Html Msg
questionnaireView appState model _ =
    div [ detailClass "Questionnaire__Edit" ]
        [ Page.header "Edit questionnaire" []
        , div []
            [ FormResult.errorOnlyView model.savingQuestionnaire
            , formView appState model.editForm |> Html.map FormMsg
            , FormActions.view
                (Questionnaires Questionnaires.Routing.Index)
                (ActionButton.ButtonConfig "Save" model.savingQuestionnaire (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form CustomFormError QuestionnaireEditForm -> Html Form.Msg
formView appState form =
    let
        accessibilitySelect =
            if appState.config.questionnaireAccessibilityEnabled then
                FormGroup.richRadioGroup QuestionnaireAccessibility.formOptions form "accessibility" "Accessibility"

            else
                emptyNode
    in
    div []
        [ FormGroup.input form "name" "Name"
        , accessibilitySelect
        ]
