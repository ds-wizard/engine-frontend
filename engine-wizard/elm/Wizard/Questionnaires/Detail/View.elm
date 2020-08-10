module Wizard.Questionnaires.Detail.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Shared.Auth.Session as Session
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l, lx)
import Shared.Utils exposing (listInsertIf)
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
import Wizard.Documents.Routes
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs as DeleteQuestionnaireModalMsg
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.QuestionnaireDescriptor as QuestionnaireDescriptor
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.View as DeleteQuestionnaireModal
import Wizard.Questionnaires.Detail.Models exposing (Model)
import Wizard.Questionnaires.Detail.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing


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
                |> listInsertIf QuestionnaireFeature.summaryReport appState.config.questionnaire.summaryReport.enabled

        questionnaireCfg =
            { features = features
            , levels =
                if appState.config.questionnaire.levels.enabled then
                    Just levels

                else
                    Nothing
            , getExtraQuestionClass = always Nothing
            , forceDisabled = False
            , createRenderer = defaultQuestionnaireRenderer appState
            }
    in
    div [ class "Questionnaires__Detail" ]
        [ questionnaireHeader appState model questionnaireModel
        , FormResult.view appState model.savingQuestionnaire
        , div [ class "questionnaire-wrapper" ]
            [ viewQuestionnaire questionnaireCfg appState questionnaireModel |> Html.map QuestionnaireMsg ]
        , Html.map DeleteQuestionnaireModalMsg <| DeleteQuestionnaireModal.view appState model.deleteModalModel
        ]


questionnaireHeader : AppState -> Model -> Wizard.Common.Questionnaire.Models.Model -> Html Msg
questionnaireHeader appState model questionnaireModel =
    let
        actions =
            if questionnaireModel.dirty then
                [ lx_ "header.unsavedChanges" appState
                , button [ onClick Discard, class "btn btn-outline-danger btn-with-loader" ]
                    [ lx_ "header.discard" appState ]
                , ActionButton.button appState <|
                    ActionButton.ButtonConfig (l_ "header.save" appState) model.savingQuestionnaire Save False
                ]

            else if Session.exists appState.session then
                [ linkTo appState
                    (Routes.QuestionnairesRoute (IndexRoute PaginationQueryString.empty))
                    [ class "link-with-icon" ]
                    [ faSet "_global.close" appState
                    , lx_ "header.close" appState
                    ]
                , linkTo appState
                    (Routes.DocumentsRoute (Wizard.Documents.Routes.CreateRoute questionnaireModel.questionnaire.uuid))
                    [ class "link-with-icon" ]
                    [ faSet "questionnaireList.createDocument" appState
                    , lx_ "header.createDocument" appState
                    ]
                , Dropdown.dropdown model.actionsDropdownState
                    { options = [ Dropdown.alignMenuRight ]
                    , toggleMsg = ActionsDropdownMsg
                    , toggleButton =
                        Dropdown.toggle [ Button.roleLink ] [ text "More" ]
                    , items =
                        [ Dropdown.anchorItem
                            [ href (Routing.toUrl appState (Routes.QuestionnairesRoute (EditRoute questionnaireModel.questionnaire.uuid))) ]
                            [ faSet "_global.edit" appState
                            , lx_ "header.edit" appState
                            ]
                        , Dropdown.divider
                        , Dropdown.anchorItem
                            [ href (Routing.toUrl appState (Routes.DocumentsRoute (Wizard.Documents.Routes.IndexRoute (Just questionnaireModel.questionnaire.uuid) PaginationQueryString.empty))) ]
                            [ faSet "questionnaireList.viewDocuments" appState
                            , lx_ "header.viewDocuments" appState
                            ]
                        , Dropdown.divider
                        , Dropdown.anchorItem
                            [ onClick (CloneQuestionnaire questionnaireModel.questionnaire) ]
                            [ faSet "questionnaireList.clone" appState
                            , lx_ "header.clone" appState
                            ]
                        , Dropdown.anchorItem
                            [ href (Routing.toUrl appState (Routes.QuestionnairesRoute (CreateMigrationRoute questionnaireModel.questionnaire.uuid))) ]
                            [ faSet "questionnaireList.createMigration" appState
                            , lx_ "header.createMigration" appState
                            ]
                        , Dropdown.divider
                        , Dropdown.anchorItem
                            [ onClick (DeleteQuestionnaireModalMsg <| DeleteQuestionnaireModalMsg.ShowHideDeleteQuestionnaire <| Just <| QuestionnaireDescriptor.fromQuestionnaireDetail questionnaireModel.questionnaire)
                            , class "text-danger"
                            ]
                            [ faSet "_global.delete" appState
                            , lx_ "header.delete" appState
                            ]
                        ]
                    }
                ]

            else
                []
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
