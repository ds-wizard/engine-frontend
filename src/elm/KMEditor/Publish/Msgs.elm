module KMEditor.Publish.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.BranchDetail exposing (BranchDetail)
import KnowledgeModels.Common.PackageDetail exposing (PackageDetail)
import KnowledgeModels.Common.Version exposing (Version)


type Msg
    = GetBranchCompleted (Result ApiError BranchDetail)
    | GetPreviousPackageCompleted (Result ApiError PackageDetail)
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PutBranchCompleted (Result ApiError ())
