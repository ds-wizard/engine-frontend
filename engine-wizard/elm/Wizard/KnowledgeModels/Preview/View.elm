module Wizard.KnowledgeModels.Preview.View exposing (view)

import ActionResult
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Version
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ActionResultView as ActionResultView
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Feature as Features
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Preview.Models exposing (Model)
import Wizard.KnowledgeModels.Preview.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        actionResult =
            ActionResult.combine model.package model.questionnaireModel
    in
    Page.actionResultView appState (viewProject appState model) actionResult


viewProject : AppState -> Model -> ( PackageDetail, Questionnaire.Model ) -> Html Msg
viewProject appState model ( package, questionnaireModel ) =
    let
        questionnaire =
            Questionnaire.view appState
                { features =
                    { feedbackEnabled = False
                    , todosEnabled = False
                    , commentsEnabled = False
                    , readonly = True
                    , toolbarEnabled = False
                    , questionLinksEnabled = False
                    }
                , renderer =
                    DefaultQuestionnaireRenderer.create appState
                        questionnaireModel.questionnaire.knowledgeModel
                        (DefaultQuestionnaireRenderer.defaultResourcePageToRoute questionnaireModel.questionnaire.packageId)
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                , isKmEditor = False
                }
                { events = [] }
                questionnaireModel
    in
    div [ class "KnowledgeModels__Preview" ]
        [ viewHeader appState model package
        , questionnaire
        ]


viewHeader : AppState -> Model -> PackageDetail -> Html Msg
viewHeader appState model package =
    let
        actions =
            if Features.projectsCreateCustom appState then
                let
                    cfg =
                        { label = gettext "Create project" appState.locale
                        , result = model.creatingQuestionnaire
                        , msg = CreateProjectMsg
                        , dangerous = False
                        }
                in
                [ ActionResultView.error model.creatingQuestionnaire
                , ActionButton.button appState cfg
                ]

            else
                []
    in
    div [ class "top-header" ]
        [ div [ class "top-header-content" ]
            [ div [ class "top-header-title", dataCy "km-preview_header_title" ] [ text <| package.name ++ ", " ++ Version.toString package.version ]
            , div [ class "top-header-actions" ] actions
            ]
        ]
