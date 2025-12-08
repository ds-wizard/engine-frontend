module Wizard.Pages.ProjectImporters.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.ProjectImporter exposing (ProjectImporter)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg ProjectImporter)
    | ToggleEnabled ProjectImporter
    | ToggleEnabledComplete (Result ApiError ())
