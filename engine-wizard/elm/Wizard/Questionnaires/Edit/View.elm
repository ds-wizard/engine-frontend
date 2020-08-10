module Wizard.Questionnaires.Edit.View exposing (formView, questionnaireView, view)

import Form exposing (Form)
import Html exposing (Html, div)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
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
        visibilitySelect =
            if appState.config.questionnaire.questionnaireVisibility.enabled then
                FormGroup.richRadioGroup appState (QuestionnaireVisibility.richFormOptions appState) form "visibility" <| lg "questionnaire.visibility" appState

            else
                emptyNode

        visibilityValue =
            (Form.getFieldAsString "visibility" form).value
                |> Maybe.andThen QuestionnaireVisibility.fromString

        sharingSelect =
            case
                ( appState.config.questionnaire.questionnaireSharing.enabled
                , visibilityValue
                )
            of
                ( _, Just PrivateQuestionnaire ) ->
                    emptyNode

                ( True, Just visibility ) ->
                    FormGroup.richRadioGroup appState (QuestionnaireSharing.richFormOptions appState visibility) form "sharing" <| lg "questionnaire.sharing" appState

                _ ->
                    emptyNode
    in
    div []
        [ FormGroup.input appState form "name" <| lg "questionnaire.name" appState
        , visibilitySelect
        , sharingSelect
        ]
