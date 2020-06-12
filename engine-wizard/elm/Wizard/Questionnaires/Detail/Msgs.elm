module Wizard.Questionnaires.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Questionnaire.Msgs
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs as DeleteQuestionnaireModal
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


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
