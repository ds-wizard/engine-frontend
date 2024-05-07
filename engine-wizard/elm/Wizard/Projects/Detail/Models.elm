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
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnaireApi
import Shared.Data.OnlineUserInfo exposing (OnlineUserInfo)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireAction exposing (QuestionnaireAction)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Shared.WebSocket as WebSocket exposing (WebSocket)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.Preview as Preview exposing (PreviewState(..))
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Projects.Detail.Components.QuestionnaireVersionViewModal as QuestionnaireVersionViewModal
import Wizard.Projects.Detail.Components.RevertModal as RevertModal
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.Models as Documents
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute exposing (ProjectDetailRoute)


type alias Model =
    { uuid : Uuid
    , mbSelectedPath : Maybe String
    , websocket : WebSocket
    , offline : Bool
    , error : Bool
    , onlineUsers : List OnlineUserInfo
    , savingActionUuids : List Uuid
    , projectSavingModel : ProjectSaving.Model
    , shareModalModel : ShareModal.Model
    , previewModel : Preview.Model
    , questionnaireModel : ActionResult Questionnaire.Model
    , questionnaireImporters : ActionResult (List QuestionnaireImporter)
    , questionnaireActions : ActionResult (List QuestionnaireAction)
    , questionnaireWebSocketDebounce : Dict String (Debounce QuestionnaireEvent)
    , summaryReportModel : SummaryReport.Model
    , documentsModel : Documents.Model
    , settingsModel : Settings.Model
    , newDocumentModel : NewDocument.Model
    , questionnaireVersionViewModalModel : QuestionnaireVersionViewModal.Model
    , revertModalModel : RevertModal.Model
    , addingToMyProjects : ActionResult ()
    }


init : AppState -> Uuid -> Maybe String -> Model
init appState uuid mbSelectedPath =
    { uuid = uuid
    , mbSelectedPath = mbSelectedPath
    , websocket = WebSocket.init (QuestionnaireApi.websocket uuid appState)
    , offline = False
    , error = False
    , onlineUsers = []
    , savingActionUuids = []
    , projectSavingModel = ProjectSaving.init
    , shareModalModel = ShareModal.init appState
    , previewModel = Preview.init uuid Preview.TemplateNotSet
    , questionnaireModel = Loading
    , questionnaireImporters = Loading
    , questionnaireActions = Loading
    , questionnaireWebSocketDebounce = Dict.empty
    , summaryReportModel = SummaryReport.init
    , documentsModel = Documents.initialModel PaginationQueryString.empty
    , newDocumentModel = NewDocument.initEmpty
    , settingsModel = Settings.init appState Nothing
    , questionnaireVersionViewModalModel = QuestionnaireVersionViewModal.initEmpty
    , revertModalModel = RevertModal.init
    , addingToMyProjects = Unset
    }


initPageModel : AppState -> ProjectDetailRoute -> Model -> Model
initPageModel appState route model =
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
                            NewDocument.initialModel qm.questionnaire mbEventUuid

                        _ ->
                            NewDocument.initEmpty
            }

        PlanDetailRoute.Settings ->
            { model | settingsModel = Settings.init appState (ActionResult.unwrap Nothing (.questionnaire >> Just) model.questionnaireModel) }

        _ ->
            model


hasTemplate : Model -> Bool
hasTemplate model =
    ActionResult.unwrap False (.questionnaire >> .documentTemplateId >> Maybe.isJust) model.questionnaireModel
        && ActionResult.unwrap False (.questionnaire >> .format >> Maybe.isJust) model.questionnaireModel


addSavingActionUuid : Uuid -> Model -> Model
addSavingActionUuid uuid model =
    { model
        | savingActionUuids = model.savingActionUuids ++ [ uuid ]
        , projectSavingModel = ProjectSaving.setSaving model.projectSavingModel
    }


addQuestionnaireEvent : QuestionnaireEvent -> Model -> Model
addQuestionnaireEvent event model =
    { model | questionnaireModel = ActionResult.map (Questionnaire.addEvent event) model.questionnaireModel }


removeSavingActionUuid : Uuid -> Model -> ( Model, Bool )
removeSavingActionUuid uuid model =
    let
        newSavingActionUuids =
            List.filter ((/=) uuid) model.savingActionUuids

        newQuestionnaireSavingModel =
            if not (List.isEmpty model.savingActionUuids) && List.isEmpty newSavingActionUuids then
                ProjectSaving.setSaved model.projectSavingModel

            else
                model.projectSavingModel
    in
    ( { model
        | savingActionUuids = newSavingActionUuids
        , projectSavingModel = newQuestionnaireSavingModel
      }
    , List.length model.savingActionUuids /= List.length newSavingActionUuids
    )
