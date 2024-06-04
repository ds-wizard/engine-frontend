module Wizard.Projects.Detail.Models exposing
    ( Model
    , addQuestionnaireEvent
    , addSavingActionUuid
    , init
    , initPageModel
    , removeSavingActionUuid
    )

import ActionResult exposing (ActionResult(..))
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Shared.Api.Questionnaires as QuestionnaireApi
import Shared.Data.OnlineUserInfo exposing (OnlineUserInfo)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireCommon exposing (QuestionnaireCommon)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnairePreview exposing (QuestionnairePreview)
import Shared.Data.QuestionnaireSettings exposing (QuestionnaireSettings)
import Shared.Data.SummaryReport exposing (SummaryReport)
import Shared.WebSocket as WebSocket exposing (WebSocket)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.Preview as Preview
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
    , questionnaireSummaryReport : ActionResult SummaryReport
    , questionnairePreview : ActionResult QuestionnairePreview
    , questionnaireSettings : ActionResult QuestionnaireSettings
    , questionnaireWebSocketDebounce : Dict String (Debounce QuestionnaireEvent)
    , documentsModel : Documents.Model
    , settingsModel : Settings.Model
    , newDocumentModel : NewDocument.Model
    , questionnaireVersionViewModalModel : QuestionnaireVersionViewModal.Model
    , revertModalModel : RevertModal.Model
    , addingToMyProjects : ActionResult ()
    , questionnaireCommon : ActionResult QuestionnaireCommon
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
    , shareModalModel = ShareModal.init
    , previewModel = Preview.init uuid Preview.TemplateNotSet
    , questionnaireModel = Loading
    , questionnaireSummaryReport = Loading
    , questionnairePreview = Loading
    , questionnaireSettings = Loading
    , questionnaireWebSocketDebounce = Dict.empty
    , documentsModel = Documents.initialModel PaginationQueryString.empty
    , newDocumentModel = NewDocument.initEmpty
    , settingsModel = Settings.init appState Nothing
    , questionnaireVersionViewModalModel = QuestionnaireVersionViewModal.initEmpty
    , revertModalModel = RevertModal.init
    , addingToMyProjects = Unset
    , questionnaireCommon = Loading
    }


initPageModel : AppState -> ProjectDetailRoute -> Model -> Model
initPageModel appState route model =
    case route of
        PlanDetailRoute.Documents paginationQueryString ->
            { model | documentsModel = Documents.initialModel paginationQueryString }

        PlanDetailRoute.NewDocument _ ->
            { model | newDocumentModel = NewDocument.initEmpty }

        PlanDetailRoute.Settings ->
            { model | settingsModel = Settings.init appState (ActionResult.toMaybe model.questionnaireSettings) }

        _ ->
            model


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
