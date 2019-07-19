module Questionnaires.Detail.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode)
import Common.Questionnaire.DefaultQuestionnaireRenderer exposing (defaultQuestionnaireRenderer)
import Common.Questionnaire.Models
import Common.Questionnaire.Models.QuestionnaireFeature as QuestionnaireFeature
import Common.Questionnaire.View exposing (viewQuestionnaire)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Common.Models.Entities exposing (Level)
import KnowledgeModels.Common.Version as Version
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Questionnaires.Detail.Models exposing (Model)
import Questionnaires.Detail.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (content appState model) <| ActionResult.combine model.questionnaireModel model.levels


content : AppState -> Model -> ( Common.Questionnaire.Models.Model, List Level ) -> Html Msg
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
            , createRenderer = defaultQuestionnaireRenderer
            }
    in
    div [ class "Questionnaires__Detail" ]
        [ questionnaireHeader model.savingQuestionnaire questionnaireModel
        , FormResult.view model.savingQuestionnaire
        , div [ class "questionnaire-wrapper" ]
            [ viewQuestionnaire questionnaireCfg appState questionnaireModel |> Html.map QuestionnaireMsg ]
        ]


questionnaireHeader : ActionResult String -> Common.Questionnaire.Models.Model -> Html Msg
questionnaireHeader savingQuestionnaire questionnaireModel =
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
                , ActionButton.button <| ActionButton.ButtonConfig "Save" savingQuestionnaire Save False
                ]
            ]
        ]


questionnaireTitle : QuestionnaireDetail -> String
questionnaireTitle questionnaire =
    questionnaire.name ++ " (" ++ questionnaire.package.name ++ ", " ++ Version.toString questionnaire.package.version ++ ")"
