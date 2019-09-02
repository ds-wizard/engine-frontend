module Questionnaires.Edit.View exposing (formView, questionnaireView, view)

import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Html exposing (emptyNode)
import Common.Html.Attribute exposing (detailClass)
import Common.Locale exposing (l, lg)
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
import Questionnaires.Routes exposing (Route(..))
import Routes


l_ : String -> AppState -> String
l_ =
    l "Questionnaires.Edit.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (questionnaireView appState model) model.questionnaire


questionnaireView : AppState -> Model -> QuestionnaireDetail -> Html Msg
questionnaireView appState model _ =
    div [ detailClass "Questionnaire__Edit" ]
        [ Page.header (l_ "header.title" appState) []
        , div []
            [ FormResult.errorOnlyView model.savingQuestionnaire
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
