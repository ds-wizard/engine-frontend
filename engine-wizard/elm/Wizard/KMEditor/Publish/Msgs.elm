module Wizard.KMEditor.Publish.Msgs exposing (Msg(..))

import Form
import Shared.Data.BranchDetail exposing (BranchDetail)
import Shared.Data.Package exposing (Package)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Error.ApiError exposing (ApiError)
import Version exposing (Version)


type Msg
    = GetBranchCompleted (Result ApiError BranchDetail)
    | GetPreviousPackageCompleted (Result ApiError PackageDetail)
    | Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PutBranchCompleted (Result ApiError Package)
