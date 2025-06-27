module Wizard.ProjectImporters.Index.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.QuestionnaireImporter exposing (QuestionnaireImporter)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg QuestionnaireImporter)
    | ToggleEnabled QuestionnaireImporter
    | ToggleEnabledComplete (Result ApiError ())
