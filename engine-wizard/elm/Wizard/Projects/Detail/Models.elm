module Wizard.Projects.Detail.Models exposing
    ( Model
    , addQuestionnaireEvent
    , addSavingActionUuid
    , hasTemplate
    , init
    , initPageModel
    , removeSavingActionUuid
    )

import ActionResult exposing (ActionResult(..))
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnaireApi
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.WebSocket as WebSocket exposing (WebSocket)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.PlanSaving as PlanSaving
import Wizard.Projects.Detail.Components.Preview as Preview exposing (PreviewState(..))
import Wizard.Projects.Detail.Components.QuestionnaireVersionViewModal as QuestionnaireVersionViewModal
import Wizard.Projects.Detail.Components.RevertModal as RevertModal
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.Models as Documents
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute exposing (ProjectDetailRoute)


type alias Model =
    { uuid : Uuid
    , websocket : WebSocket
    , offline : Bool
    , error : Bool
    , onlineUsers : List OnlineUser.Model
    , savingActionUuids : List Uuid
    , planSavingModel : PlanSaving.Model
    , shareModalModel : ShareModal.Model
    , previewModel : Preview.Model
    , questionnaireModel : ActionResult Questionnaire.Model
    , summaryReportModel : SummaryReport.Model
    , documentsModel : Documents.Model
    , settingsModel : Settings.Model
    , newDocumentModel : NewDocument.Model
    , questionnaireVersionViewModalModel : QuestionnaireVersionViewModal.Model
    , revertModalModel : RevertModal.Model
    , addingToMyProjects : ActionResult ()
    }


init : AppState -> Uuid -> Model
init appState uuid =
    { uuid = uuid
    , websocket = WebSocket.init (QuestionnaireApi.websocket uuid appState)
    , offline = False
    , error = False
    , onlineUsers = []
    , savingActionUuids = []
    , planSavingModel = PlanSaving.init
    , shareModalModel = ShareModal.init
    , previewModel = Preview.init uuid Preview.TemplateNotSet
    , questionnaireModel = Loading
    , summaryReportModel = SummaryReport.init
    , documentsModel = Documents.initialModel PaginationQueryString.empty
    , newDocumentModel = NewDocument.initEmpty
    , settingsModel = Settings.init Nothing
    , questionnaireVersionViewModalModel = QuestionnaireVersionViewModal.initEmpty
    , revertModalModel = RevertModal.init
    , addingToMyProjects = Unset
    }


initPageModel : ProjectDetailRoute -> Model -> Model
initPageModel route model =
    case route of
        PlanDetailRoute.Preview ->
            let
                state =
                    if hasTemplate model then
                        Preview Loading

                    else
                        TemplateNotSet
            in
            { model | previewModel = Preview.init model.uuid state }

        PlanDetailRoute.Metrics ->
            { model | summaryReportModel = SummaryReport.init }

        PlanDetailRoute.Documents paginationQueryString ->
            { model | documentsModel = Documents.initialModel paginationQueryString }

        PlanDetailRoute.NewDocument mbEventUuid ->
            { model
                | newDocumentModel =
                    case model.questionnaireModel of
                        Success qm ->
                            NewDocument.initialModel qm.questionnaire (Maybe.andThen Uuid.fromString mbEventUuid)

                        _ ->
                            NewDocument.initEmpty
            }

        PlanDetailRoute.Settings ->
            { model | settingsModel = Settings.init (ActionResult.unwrap Nothing (.questionnaire >> Just) model.questionnaireModel) }

        _ ->
            model


hasTemplate : Model -> Bool
hasTemplate model =
    ActionResult.unwrap False (.questionnaire >> .templateId >> Maybe.isJust) model.questionnaireModel
        && ActionResult.unwrap False (.questionnaire >> .format >> Maybe.isJust) model.questionnaireModel


addSavingActionUuid : Uuid -> Model -> Model
addSavingActionUuid uuid model =
    { model
        | savingActionUuids = model.savingActionUuids ++ [ uuid ]
        , planSavingModel = PlanSaving.setSaving model.planSavingModel
    }


addQuestionnaireEvent : QuestionnaireEvent -> Model -> Model
addQuestionnaireEvent event model =
    let
        setEvent questionnaireModel =
            let
                questionnaire =
                    questionnaireModel.questionnaire
            in
            { questionnaireModel
                | questionnaire = { questionnaire | events = questionnaire.events ++ [ event ] }
            }
    in
    { model | questionnaireModel = ActionResult.map setEvent model.questionnaireModel }


removeSavingActionUuid : Uuid -> Model -> ( Model, Bool )
removeSavingActionUuid uuid model =
    let
        newSavingActionUuids =
            List.filter ((/=) uuid) model.savingActionUuids

        newQuestionnaireSavingModel =
            if not (List.isEmpty model.savingActionUuids) && List.isEmpty newSavingActionUuids then
                PlanSaving.setSaved model.planSavingModel

            else
                model.planSavingModel
    in
    ( { model
        | savingActionUuids = newSavingActionUuids
        , planSavingModel = newQuestionnaireSavingModel
      }
    , List.length model.savingActionUuids /= List.length newSavingActionUuids
    )
