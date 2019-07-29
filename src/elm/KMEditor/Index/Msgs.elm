module KMEditor.Index.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.Branch exposing (Branch)
import KnowledgeModels.Common.PackageDetail exposing (PackageDetail)


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
