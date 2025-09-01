module Wizard.Pages.KMEditor.Index.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Uuid exposing (Uuid)
import Wizard.Api.Models.Branch exposing (Branch)
import Wizard.Components.Listing.Msgs as Listing
import Wizard.Pages.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.Pages.KMEditor.Common.UpgradeModal as UpgradeModal


type Msg
    = DeleteMigration Uuid
    | DeleteMigrationCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Branch)
    | DeleteModalMsg DeleteModal.Msg
    | UpgradeModalMsg UpgradeModal.Msg
