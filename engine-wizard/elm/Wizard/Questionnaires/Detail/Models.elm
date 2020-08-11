module Wizard.Questionnaires.Detail.Models exposing
    ( Model
    , addSavingActionUuid
    , initialModel
    , removeSavingActionUuid
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Shared.Api.Questionnaires as QuestionnaireApi
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.WebSocket as WebSocket exposing (WebSocket)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Questionnaires.Common.CloneQuestionnaireModal.Models as CloneQuestionnaireModal
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Models as DeleteQuestionnaireModal
import Wizard.Questionnaires.Detail.Components.QuestionnaireSaving as QuestionnaireSaving


type alias Model =
    { uuid : Uuid
    , levels : ActionResult (List Level)
    , metrics : ActionResult (List Metric)
    , websocket : WebSocket
    , offline : Bool
    , error : Bool
    , onlineUsers : List OnlineUser.Model
    , actionsDropdownState : Dropdown.State
    , questionnaireSavingModel : QuestionnaireSaving.Model
    , savingActionUuids : List Uuid
    , questionnaireModel : ActionResult Questionnaire.Model
    , deleteModalModel : DeleteQuestionnaireModal.Model
    , cloneModalModel : CloneQuestionnaireModal.Model
    }


initialModel : AppState -> Uuid -> Model
initialModel appState uuid =
    { uuid = uuid
    , levels = Loading
    , metrics = Loading
    , websocket = WebSocket.init (QuestionnaireApi.websocket uuid appState)
    , offline = False
    , error = False
    , onlineUsers = []
    , actionsDropdownState = Dropdown.initialState
    , questionnaireSavingModel = QuestionnaireSaving.init
    , savingActionUuids = []
    , questionnaireModel = Loading
    , deleteModalModel = DeleteQuestionnaireModal.initialModel
    , cloneModalModel = CloneQuestionnaireModal.initialModel
    }


addSavingActionUuid : Uuid -> Model -> Model
addSavingActionUuid uuid model =
    { model
        | savingActionUuids = model.savingActionUuids ++ [ uuid ]
        , questionnaireSavingModel = QuestionnaireSaving.setSaving model.questionnaireSavingModel
    }


removeSavingActionUuid : Uuid -> Model -> ( Model, Bool )
removeSavingActionUuid uuid model =
    let
        newSavingActionUuids =
            List.filter ((/=) uuid) model.savingActionUuids

        newQuestionnaireSavingModel =
            if not (List.isEmpty model.savingActionUuids) && List.isEmpty newSavingActionUuids then
                QuestionnaireSaving.setSaved model.questionnaireSavingModel

            else
                model.questionnaireSavingModel
    in
    ( { model
        | savingActionUuids = newSavingActionUuids
        , questionnaireSavingModel = newQuestionnaireSavingModel
      }
    , List.length model.savingActionUuids /= List.length newSavingActionUuids
    )
