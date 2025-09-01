module Wizard.Pages.KnowledgeModels.Preview.View exposing (view)

import ActionResult
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Shared.Components.ActionButton as ActionButton
import Shared.Components.Page as Page
import Version
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Components.ActionResultView as ActionResultView
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Preview.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Preview.Msgs exposing (Msg(..))
import Wizard.Utils.Feature as Features


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
                { events = []
                , branchUuid = Nothing
                }
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
                , ActionButton.button cfg
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
