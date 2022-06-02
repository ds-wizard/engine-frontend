module Wizard.KMEditor.Index.Msgs exposing (Msg(..))

import Shared.Data.Branch exposing (Branch)
import Shared.Error.ApiError exposing (ApiError)
import Uuid exposing (Uuid)
import Wizard.Common.Components.Listing.Msgs as Listing
import Wizard.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.KMEditor.Common.UpgradeModal as UpgradeModal


type Msg
    = DeleteMigration Uuid
    | DeleteMigrationCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Branch)
    | DeleteModalMsg DeleteModal.Msg
    | UpgradeModalMsg UpgradeModal.Msg
