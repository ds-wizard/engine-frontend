module Wizard.Questionnaires.Detail.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html exposing (Html, button, div, i, p, text)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Shared.Auth.Session as Session
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.Page as Page
import Wizard.Documents.Routes
import Wizard.Questionnaires.Common.CloneQuestionnaireModal.Msgs as CloneQuestionnaireModalMsg
import Wizard.Questionnaires.Common.CloneQuestionnaireModal.View as CloneQuestionnaireModal
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs as DeleteQuestionnaireModalMsg
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.View as DeleteQuestionnaireModal
import Wizard.Questionnaires.Common.QuestionnaireDescriptor as QuestionnaireDescriptor
import Wizard.Questionnaires.Common.View exposing (visibilityIcons)
import Wizard.Questionnaires.Detail.Components.QuestionnaireSaving as QuestionnaireSaving
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


type QuestionViewState
    = Default
    | Answered
    | Desirable


view : AppState -> Model -> Html Msg
view appState model =
    if model.error then
        viewError appState

    else if model.offline then
        viewOffline appState

    else
        let
            results =
                ActionResult.combine3 model.questionnaireModel model.levels model.metrics
        in
        Page.actionResultView appState (viewContent appState model) results


viewContent : AppState -> Model -> ( Questionnaire.Model, List Level, List Metric ) -> Html Msg
viewContent appState model ( qm, levels, metrics ) =
    div [ class "Questionnaires__Detail" ]
        [ viewHeader appState model qm
        , viewQuestionnaire appState qm levels metrics
        , Html.map DeleteQuestionnaireModalMsg <| DeleteQuestionnaireModal.view appState model.deleteModalModel
        , Html.map CloneQuestionnaireModalMsg <| CloneQuestionnaireModal.view appState model.cloneModalModel
        ]


viewOffline : AppState -> Html Msg
viewOffline appState =
    Page.illustratedMessageHtml
        { image = "warning"
        , heading = l_ "offline.heading" appState
        , content =
            [ p [] [ lx_ "offline.text" appState ]
            , p [] [ button [ onClick Refresh, class "btn btn-lg btn-primary" ] [ lx_ "offline.refresh" appState ] ]
            ]
        }


viewError : AppState -> Html Msg
viewError appState =
    Page.illustratedMessageHtml
        { image = "warning"
        , heading = l_ "error.heading" appState
        , content =
            [ p [] [ lx_ "error.text" appState ]
            , p [] [ button [ onClick Refresh, class "btn btn-lg btn-primary" ] [ lx_ "error.refresh" appState ] ]
            ]
        }



-- HEADER


viewHeader : AppState -> Model -> Questionnaire.Model -> Html Msg
viewHeader appState model qm =
    div [ class "questionnaire-header" ]
        [ div [ class "questionnaire-header__section" ]
            (viewHeaderTitle qm
                :: visibilityIcons appState qm.questionnaire
                ++ [ viewHeaderQuestionnaireSaving appState model ]
            )
        , div [ class "questionnaire-header__section" ]
            [ viewHeaderOnlineUsers appState model
            , viewHeaderActions appState model qm
            ]
        ]


viewHeaderTitle : Questionnaire.Model -> Html Msg
viewHeaderTitle qm =
    div [ class "questionnaire-header__title" ]
        [ text qm.questionnaire.name ]


viewHeaderQuestionnaireSaving : AppState -> Model -> Html Msg
viewHeaderQuestionnaireSaving appState model =
    Html.map QuestionnaireSavingMsg <|
        QuestionnaireSaving.view appState model.questionnaireSavingModel


viewHeaderOnlineUsers : AppState -> Model -> Html Msg
viewHeaderOnlineUsers appState model =
    if List.isEmpty model.onlineUsers then
        emptyNode

    else
        let
            extraUsers =
                if List.length model.onlineUsers > 10 then
                    div [ class "extra-users-count" ]
                        [ text ("+" ++ String.fromInt (List.length model.onlineUsers - 10)) ]

                else
                    emptyNode
        in
        div [ class "questionnaire-header__online-users", classList [ ( "questionnaire-header__online-users--stacked", List.length model.onlineUsers > 5 ) ] ]
            (List.indexedMap (\i u -> Html.map (OnlineUserMsg i) (OnlineUser.view appState u)) (List.take 10 model.onlineUsers)
                ++ [ extraUsers ]
            )


viewHeaderActions : AppState -> Model -> Questionnaire.Model -> Html Msg
viewHeaderActions appState model qm =
    if Session.exists appState.session then
        div [ class "questionnaire-header__actions" ]
            [ linkTo appState
                (Routes.DocumentsRoute (Wizard.Documents.Routes.CreateRoute model.uuid))
                [ class "link-with-icon" ]
                [ faSet "questionnaireList.createDocument" appState
                , lx_ "header.createDocument" appState
                ]
            , Dropdown.dropdown model.actionsDropdownState
                { options = [ Dropdown.alignMenuRight ]
                , toggleMsg = ActionsDropdownMsg
                , toggleButton =
                    Dropdown.toggle [ Button.roleLink ] [ lx_ "header.more" appState ]
                , items =
                    [ Dropdown.anchorItem
                        [ href (Routing.toUrl appState (Routes.QuestionnairesRoute (EditRoute model.uuid))) ]
                        [ faSet "_global.edit" appState
                        , lx_ "header.edit" appState
                        ]
                    , Dropdown.divider
                    , Dropdown.anchorItem
                        [ href (Routing.toUrl appState (Routes.DocumentsRoute (Wizard.Documents.Routes.IndexRoute (Just model.uuid) PaginationQueryString.empty))) ]
                        [ faSet "questionnaireList.viewDocuments" appState
                        , lx_ "header.viewDocuments" appState
                        ]
                    , Dropdown.divider
                    , Dropdown.anchorItem
                        [ onClick (CloneQuestionnaireModalMsg <| CloneQuestionnaireModalMsg.ShowHideCloneQuestionnaire <| Just <| QuestionnaireDescriptor.fromQuestionnaireDetail qm.questionnaire) ]
                        [ faSet "questionnaireList.clone" appState
                        , lx_ "header.clone" appState
                        ]
                    , Dropdown.anchorItem
                        [ href (Routing.toUrl appState (Routes.QuestionnairesRoute (CreateMigrationRoute model.uuid))) ]
                        [ faSet "questionnaireList.createMigration" appState
                        , lx_ "header.createMigration" appState
                        ]
                    , Dropdown.divider
                    , Dropdown.anchorItem
                        [ onClick (DeleteQuestionnaireModalMsg <| DeleteQuestionnaireModalMsg.ShowHideDeleteQuestionnaire <| Just <| QuestionnaireDescriptor.fromQuestionnaireDetail qm.questionnaire)
                        , class "text-danger"
                        ]
                        [ faSet "_global.delete" appState
                        , lx_ "header.delete" appState
                        ]
                    ]
                }
            ]

    else
        emptyNode



-- QUESTIONNAIRE


viewQuestionnaire : AppState -> Questionnaire.Model -> List Level -> List Metric -> Html Msg
viewQuestionnaire appState qm levels metrics =
    let
        isEditable =
            QuestionnaireDetail.isEditable appState qm.questionnaire
    in
    Html.map QuestionnaireMsg <|
        Questionnaire.view appState
            { features =
                { feedbackEnabled = True
                , summaryReportEnabled = True
                , todosEnabled = isEditable
                , todoListEnabled = isEditable
                , readonly = not isEditable
                }
            , renderer = DefaultQuestionnaireRenderer.create appState qm.questionnaire.knowledgeModel levels metrics
            }
            { levels = levels
            , metrics = metrics
            }
            qm
