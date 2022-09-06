module Wizard.ProjectImporters.Index.Msgs exposing (Msg(..))

import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg QuestionnaireImporter)
    | ToggleEnabled QuestionnaireImporter
    | ToggleEnabledComplete (Result ApiError ())
