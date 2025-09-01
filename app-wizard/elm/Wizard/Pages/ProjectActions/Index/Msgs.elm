module Wizard.Pages.ProjectActions.Index.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.QuestionnaireAction exposing (QuestionnaireAction)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg QuestionnaireAction)
    | ToggleEnabled QuestionnaireAction
    | ToggleEnabledComplete (Result ApiError ())
