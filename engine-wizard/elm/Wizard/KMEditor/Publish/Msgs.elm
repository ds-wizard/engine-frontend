module Wizard.KMEditor.Publish.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Version exposing (Version)
import Wizard.KMEditor.Common.BranchDetail exposing (BranchDetail)
import Wizard.KnowledgeModels.Common.PackageDetail exposing (PackageDetail)


type Msg
    = GetBranchCompleted (Result ApiError BranchDetail)
    | GetPreviousPackageCompleted (Result ApiError PackageDetail)
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PutBranchCompleted (Result ApiError ())
