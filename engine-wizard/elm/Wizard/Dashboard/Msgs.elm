module Wizard.Dashboard.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing as Listing
import Wizard.Common.Pagination.Pagination exposing (Pagination)
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)


type Msg
    = GetLevelsCompleted (Result ApiError (List Level))
    | GetQuestionnairesCompleted (Result ApiError (Pagination Questionnaire))
    | ListingMsg Listing.Msg
