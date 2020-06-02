module Wizard.Questionnaires.Detail.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Shared.Locale exposing (l, lx)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
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
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Utils exposing (listInsertIf)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Questionnaires.Detail.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Questionnaires.Detail.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (content appState model) <| ActionResult.combine model.questionnaireModel model.levels


content : AppState -> Model -> ( Wizard.Common.Questionnaire.Models.Model, List Level ) -> Html Msg
content appState model ( questionnaireModel, levels ) =
    let
        features =
            [ QuestionnaireFeature.feedback
            , QuestionnaireFeature.todos
            , QuestionnaireFeature.todoList
            ]
                |> listInsertIf QuestionnaireFeature.summaryReport appState.config.questionnaires.summaryReport.enabled

        questionnaireCfg =
            { features = features
            , levels =
                if appState.config.questionnaires.levels.enabled then
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
        actions =
            if questionnaireModel.dirty then
                [ lx_ "header.unsavedChanges" appState
                , button [ onClick Discard, class "btn btn-outline-danger btn-with-loader" ]
                    [ lx_ "header.discard" appState ]
                , ActionButton.button appState <|
                    ActionButton.ButtonConfig (l_ "header.save" appState) savingQuestionnaire Save False
                ]

            else
                [ linkTo appState
                    (Routes.QuestionnairesRoute IndexRoute)
                    [ class "btn btn-outline-primary btn-with-loader" ]
                    [ lx_ "header.close" appState ]
                ]
    in
    div [ class "top-header" ]
        [ div [ class "top-header-content" ]
            [ div [ class "top-header-title" ] [ text <| questionnaireTitle questionnaireModel.questionnaire ]
            , div [ class "top-header-actions" ]
                actions
            ]
        ]


questionnaireTitle : QuestionnaireDetail -> String
questionnaireTitle questionnaire =
    questionnaire.name ++ " (" ++ questionnaire.package.name ++ ", " ++ Version.toString questionnaire.package.version ++ ")"
