module Wizard.KMEditor.Index.Msgs exposing (Msg(..))

import Form
import Shared.Data.Branch exposing (Branch)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Error.ApiError exposing (ApiError)
import Uuid exposing (Uuid)
import Wizard.Common.Components.Listing as Listing


type Msg
    = GetBranchesCompleted (Result ApiError (List Branch))
    | ShowHideDeleteBranchModal (Maybe Branch)
    | DeleteBranch
    | DeleteBranchCompleted (Result ApiError ())
    | PostMigrationCompleted (Result ApiError ())
    | ShowHideUpgradeModal (Maybe Branch)
    | GetPackageCompleted (Result ApiError PackageDetail)
    | UpgradeFormMsg Form.Msg
    | DeleteMigration Uuid
    | DeleteMigrationCompleted (Result ApiError ())
    | ListingMsg Listing.Msg
