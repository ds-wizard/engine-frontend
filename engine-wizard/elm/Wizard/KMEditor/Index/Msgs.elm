module Wizard.KMEditor.Index.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing as Listing
import Wizard.KMEditor.Common.Branch exposing (Branch)
import Wizard.KnowledgeModels.Common.PackageDetail exposing (PackageDetail)


type Msg
    = GetBranchesCompleted (Result ApiError (List Branch))
    | ShowHideDeleteBranchModal (Maybe Branch)
    | DeleteBranch
    | DeleteBranchCompleted (Result ApiError ())
    | PostMigrationCompleted (Result ApiError ())
    | ShowHideUpgradeModal (Maybe Branch)
    | GetPackageCompleted (Result ApiError PackageDetail)
    | UpgradeFormMsg Form.Msg
    | DeleteMigration String
    | DeleteMigrationCompleted (Result ApiError ())
    | ListingMsg Listing.Msg
