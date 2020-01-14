module Wizard.Dashboard.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)


type Msg
    = GetLevelsCompleted (Result ApiError (List Level))
    | GetQuestionnairesCompleted (Result ApiError (List Questionnaire))
