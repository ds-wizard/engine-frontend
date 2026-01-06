module Wizard.Pages.ProjectActions.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.ProjectAction exposing (ProjectAction)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg ProjectAction)
    | ToggleEnabled ProjectAction
    | ToggleEnabledComplete (Result ApiError ())
