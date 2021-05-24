module Wizard.KnowledgeModels.Preview.View exposing (..)

import ActionResult
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.Package exposing (Package)
import Shared.Data.QuestionnaireDetail
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Preview.Models exposing (Model)
import Wizard.KnowledgeModels.Preview.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        actionResult =
            ActionResult.combine3 model.levels model.metrics model.questionnaireModel
    in
    Page.actionResultView appState (viewProject appState) actionResult


viewProject : AppState -> ( List Level, List Metric, Questionnaire.Model ) -> Html Msg
viewProject appState ( levels, metrics, questionnaireModel ) =
    let
        questionnaire =
            Questionnaire.view appState
                { features =
                    { feedbackEnabled = False
                    , todosEnabled = False
                    , readonly = True
                    , toolbarEnabled = False
                    }
                , renderer = DefaultQuestionnaireRenderer.create appState questionnaireModel.questionnaire.knowledgeModel levels metrics
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                }
                { levels = levels, metrics = metrics, events = [] }
                questionnaireModel
    in
    div [ class "KnowledgeModels__Project" ]
        [ viewHeader questionnaireModel.questionnaire.package
        , questionnaire
        ]


viewHeader : Package -> Html Msg
viewHeader package =
    div [ class "top-header" ]
        [ div [ class "top-header-content" ]
            [ div [ class "top-header-title" ]
                [ text <| package.name ++ ", " ++ Version.toString package.version ]
            ]
        ]
