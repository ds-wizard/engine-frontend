module Wizard.Questionnaires.Detail.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (emptyNode)
import Wizard.Common.Questionnaire.DefaultQuestionnaireRenderer exposing (defaultQuestionnaireRenderer)
import Wizard.Common.Questionnaire.Models
import Wizard.Common.Questionnaire.Models.QuestionnaireFeature as QuestionnaireFeature
import Wizard.Common.Questionnaire.View exposing (viewQuestionnaire)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Detail.Models exposing (Model)
import Wizard.Questionnaires.Detail.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (content appState model) <| ActionResult.combine model.questionnaireModel model.levels


content : AppState -> Model -> ( Wizard.Common.Questionnaire.Models.Model, List Level ) -> Html Msg
content appState model ( questionnaireModel, levels ) =
    let
        questionnaireCfg =
            { features =
                [ QuestionnaireFeature.feedback
                , QuestionnaireFeature.summaryReport
                , QuestionnaireFeature.todos
                , QuestionnaireFeature.todoList
                ]
            , levels =
                if appState.config.levelsEnabled then
                    Just levels

                else
                    Nothing
            , getExtraQuestionClass = always Nothing
            , forceDisabled = False
            , createRenderer = defaultQuestionnaireRenderer appState
            }
    in
    div [ class "Questionnaires__Detail" ]
        [ questionnaireHeader appState model.savingQuestionnaire questionnaireModel
        , FormResult.view appState model.savingQuestionnaire
        , div [ class "questionnaire-wrapper" ]
            [ viewQuestionnaire questionnaireCfg appState questionnaireModel |> Html.map QuestionnaireMsg ]
        ]


questionnaireHeader : AppState -> ActionResult String -> Wizard.Common.Questionnaire.Models.Model -> Html Msg
questionnaireHeader appState savingQuestionnaire questionnaireModel =
    let
        unsavedChanges =
            if questionnaireModel.dirty then
                text "(unsaved changes)"

            else
                emptyNode
    in
    div [ class "top-header" ]
        [ div [ class "top-header-content" ]
            [ div [ class "top-header-title" ] [ text <| questionnaireTitle questionnaireModel.questionnaire ]
            , div [ class "top-header-actions" ]
                [ unsavedChanges
                , ActionButton.button appState <| ActionButton.ButtonConfig "Save" savingQuestionnaire Save False
                ]
            ]
        ]


questionnaireTitle : QuestionnaireDetail -> String
questionnaireTitle questionnaire =
    questionnaire.name ++ " (" ++ questionnaire.package.name ++ ", " ++ Version.toString questionnaire.package.version ++ ")"
