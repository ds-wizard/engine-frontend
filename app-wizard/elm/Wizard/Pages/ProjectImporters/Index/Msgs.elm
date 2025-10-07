module Wizard.Pages.ProjectImporters.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.QuestionnaireImporter exposing (QuestionnaireImporter)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg QuestionnaireImporter)
    | ToggleEnabled QuestionnaireImporter
    | ToggleEnabledComplete (Result ApiError ())
