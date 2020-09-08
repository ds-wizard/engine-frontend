module Wizard.Questionnaires.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Error.ApiError exposing (ApiError)
import Shared.WebSocket as WebSocket
import Time
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Questionnaires.Common.CloneQuestionnaireModal.Msgs as CloneQuestionnaireModal
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs as DeleteQuestionnaireModal
import Wizard.Questionnaires.Detail.Components.QuestionnaireSaving as QuestionnaireSaving


type Msg
    = GetQuestionnaireComplete (Result ApiError QuestionnaireDetail)
    | GetLevelsComplete (Result ApiError (List Level))
    | GetMetricsComplete (Result ApiError (List Metric))
    | WebSocketMsg WebSocket.RawMsg
    | WebSocketPing Time.Posix
    | OnlineUserMsg Int OnlineUser.Msg
    | ActionsDropdownMsg Dropdown.State
    | QuestionnaireSavingMsg QuestionnaireSaving.Msg
    | QuestionnaireMsg Questionnaire.Msg
    | DeleteQuestionnaireModalMsg DeleteQuestionnaireModal.Msg
    | CloneQuestionnaireModalMsg CloneQuestionnaireModal.Msg
    | Refresh
