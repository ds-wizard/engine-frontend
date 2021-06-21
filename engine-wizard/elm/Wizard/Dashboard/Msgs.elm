module Wizard.Dashboard.Msgs exposing (Msg(..))

import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing as Listing


type Msg
    = GetQuestionnairesCompleted (Result ApiError (Pagination Questionnaire))
    | ListingMsg Listing.Msg
