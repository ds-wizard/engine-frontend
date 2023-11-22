module Wizard.ProjectActions.Index.Msgs exposing (Msg(..))

import Shared.Data.QuestionnaireAction exposing (QuestionnaireAction)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg QuestionnaireAction)
    | ToggleEnabled QuestionnaireAction
    | ToggleEnabledComplete (Result ApiError ())
