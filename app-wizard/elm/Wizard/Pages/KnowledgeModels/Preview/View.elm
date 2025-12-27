module Wizard.Pages.KnowledgeModels.Preview.View exposing (view)

import ActionResult
import Common.Components.ActionButton as ActionButton
import Common.Components.Page as Page
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Version
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
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
            ActionResult.combine model.knowledgeModelPackage model.questionnaireModel
    in
    Page.actionResultView appState (viewProject appState model) actionResult


viewProject : AppState -> Model -> ( KnowledgeModelPackageDetail, Questionnaire.Model ) -> Html Msg
viewProject appState model ( kmPackage, questionnaireModel ) =
    let
        questionnaire =
            Questionnaire.view appState
                { features =
                    { feedbackEnabled = False
                    , todosEnabled = False
                    , commentsEnabled = False
                    , pluginsEnabled = False
                    , readonly = True
                    , toolbarEnabled = False
                    , questionLinksEnabled = False
                    }
                , renderer =
                    DefaultQuestionnaireRenderer.create appState
                        (DefaultQuestionnaireRenderer.config questionnaireModel.questionnaire)
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                , isKmEditor = False
                , projectCommon = Nothing
                }
                { events = []
                , kmEditorUuid = Nothing
                }
                questionnaireModel
    in
    div [ class "KnowledgeModels__Preview" ]
        [ viewHeader appState model kmPackage
        , questionnaire
        ]


viewHeader : AppState -> Model -> KnowledgeModelPackageDetail -> Html Msg
viewHeader appState model kmPackage =
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
            [ div [ class "top-header-title", dataCy "km-preview_header_title" ] [ text <| kmPackage.name ++ ", " ++ Version.toString kmPackage.version ]
            , div [ class "top-header-actions" ] actions
            ]
        ]
