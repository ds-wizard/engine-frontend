module Wizard.Questionnaires.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Questionnaire.Msgs
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs as DeleteQuestionnaireModal


type Msg
    = GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | GetLevelsCompleted (Result ApiError (List Level))
    | GetMetricsCompleted (Result ApiError (List Metric))
    | QuestionnaireMsg Wizard.Common.Questionnaire.Msgs.Msg
    | Save
    | PutRepliesCompleted (Result ApiError ())
    | Discard
    | ActionsDropdownMsg Dropdown.State
    | DeleteQuestionnaireModalMsg DeleteQuestionnaireModal.Msg
    | CloneQuestionnaire QuestionnaireDetail
    | CloneQuestionnaireCompleted (Result ApiError Questionnaire)
