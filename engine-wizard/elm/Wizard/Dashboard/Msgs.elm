module Wizard.Dashboard.Msgs exposing (Msg(..))

import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetQuestionnairesCompleted (Result ApiError (Pagination Questionnaire))
