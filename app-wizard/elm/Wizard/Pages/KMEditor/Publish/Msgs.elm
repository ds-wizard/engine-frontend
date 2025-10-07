module Wizard.Pages.KMEditor.Publish.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Form
import Version exposing (Version)
import Wizard.Api.Models.BranchDetail exposing (BranchDetail)
import Wizard.Api.Models.Package exposing (Package)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)


type Msg
    = GetBranchCompleted (Result ApiError BranchDetail)
    | GetPreviousPackageCompleted (Result ApiError PackageDetail)
    | Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PutBranchCompleted (Result ApiError Package)
