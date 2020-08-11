module Wizard.Questionnaires.Edit.View exposing (formView, questionnaireView, view)

import Form exposing (Form)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class, classList)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnairePermission as QuestionnairePermission
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lgh)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
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
                (Routes.QuestionnairesRoute (IndexRoute PaginationQueryString.empty))
                (ActionButton.ButtonConfig (l_ "header.save" appState) model.savingQuestionnaire (FormMsg Form.Submit) False)
            ]
        ]


formView : AppState -> Form FormError QuestionnaireEditForm -> Html Form.Msg
formView appState form =
    let
        visibilityEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "visibilityEnabled" form).value

        visibilityEnabledInput =
            if appState.config.questionnaire.questionnaireVisibility.enabled then
                FormGroup.toggle form "visibilityEnabled" (lg "questionnaire.visibility" appState)

            else
                emptyNode

        visibilityPermissionInput =
            if appState.config.questionnaire.questionnaireVisibility.enabled then
                div
                    [ class "form-group form-group-toggle-extra"
                    , classList [ ( "visible", visibilityEnabled ) ]
                    ]
                    (lgh "questionnaire.visibilityPermission" [ visibilitySelect ] appState)

            else
                emptyNode

        visibilitySelect =
            if (Form.getFieldAsString "sharingPermission" form).value == Just "edit" then
                strong [] [ text "edit" ]

            else
                FormExtra.inlineSelect (QuestionnairePermission.formOptions appState) form "visibilityPermission"

        sharingEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "sharingEnabled" form).value

        sharingEnabledInput =
            if appState.config.questionnaire.questionnaireSharing.enabled then
                FormGroup.toggle form "sharingEnabled" (lg "questionnaire.sharing" appState)

            else
                emptyNode

        sharingPermissionInput =
            if appState.config.questionnaire.questionnaireSharing.enabled then
                div
                    [ class "form-group form-group-toggle-extra"
                    , classList [ ( "visible", sharingEnabled ) ]
                    ]
                    (lgh "questionnaire.sharingPermission" [ sharingSelect ] appState)

            else
                emptyNode

        sharingSelect =
            FormExtra.inlineSelect (QuestionnairePermission.formOptions appState) form "sharingPermission"
    in
    div []
        [ FormGroup.input appState form "name" <| lg "questionnaire.name" appState
        , visibilityEnabledInput
        , visibilityPermissionInput
        , sharingEnabledInput
        , sharingPermissionInput
        ]
