module Questionnaires.Edit.View exposing (formView, questionnaireView, view)

import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Html exposing (emptyNode)
import Common.Html.Attribute exposing (detailClass)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Common.View.ActionButton as ActionButton
import Common.View.FormActions as FormActions
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (Html, div)
import Msgs
import Questionnaires.Common.Models.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility)
import Questionnaires.Edit.Models exposing (Model, QuestionnaireEditForm)
import Questionnaires.Edit.Msgs exposing (Msg(..))
import Questionnaires.Routing
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    Page.actionResultView (questionnaireView wrapMsg appState model) model.questionnaire


questionnaireView : (Msg -> Msgs.Msg) -> AppState -> Model -> QuestionnaireDetail -> Html Msgs.Msg
questionnaireView wrapMsg appState model _ =
    div [ detailClass "Questionnaire__Edit" ]
        [ Page.header "Edit questionnaire" []
        , div []
            [ FormResult.errorOnlyView model.savingQuestionnaire
            , formView appState model.editForm |> Html.map (wrapMsg << FormMsg)
            , FormActions.view
                (Questionnaires Questionnaires.Routing.Index)
                (ActionButton.ButtonConfig "Save" model.savingQuestionnaire (wrapMsg <| FormMsg Form.Submit) False)
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
