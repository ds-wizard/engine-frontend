module Dashboard.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Questionnaires.Common.Questionnaire exposing (Questionnaire)


type Msg
    = GetLevelsCompleted (Result ApiError (List Level))
    | GetQuestionnairesCompleted (Result ApiError (List Questionnaire))
