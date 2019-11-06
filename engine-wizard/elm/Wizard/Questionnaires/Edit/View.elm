module Wizard.Questionnaires.Edit.View exposing (formView, questionnaireView, view)

import Form exposing (Form)
import Html exposing (Html, div)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Html exposing (emptyNode)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.Locale exposing (l, lg)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Common.QuestionnaireEditForm exposing (QuestionnaireEditForm)
import Wizard.Questionnaires.Edit.Models exposing (Model)
import Wizard.Questionnaires.Edit.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Questionnaires.Edit.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (questionnaireView appState model) model.questionnaire


questionnaireView : AppState -> Model -> QuestionnaireDetail -> Html Msg
questionnaireView appState model _ =
    div [ detailClass "Questionnaire__Edit" ]
        [ Page.header (l_ "header.title" appState) []
        , div []
            [ FormResult.errorOnlyView appState model.savingQuestionnaire
            , formView appState model.editForm |> Html.map FormMsg
            , FormActions.view appState
                (Routes.QuestionnairesRoute IndexRoute)
                (ActionButton.ButtonConfig (l_ "header.save" appState) model.savingQuestionnaire (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form CustomFormError QuestionnaireEditForm -> Html Form.Msg
formView appState form =
    let
        accessibilitySelect =
            if appState.config.questionnaireAccessibilityEnabled then
                FormGroup.richRadioGroup appState (QuestionnaireAccessibility.formOptions appState) form "accessibility" <| lg "questionnaire.accessibility" appState

            else
                emptyNode
    in
    div []
        [ FormGroup.input appState form "name" <| lg "questionnaire.name" appState
        , accessibilitySelect
        ]
