module KMEditor.Publish.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.Models exposing (KnowledgeModelDetail)
import KnowledgeModels.Common.PackageDetail exposing (PackageDetail)
import KnowledgeModels.Common.Version exposing (Version)


type Msg
    = GetKnowledgeModelCompleted (Result ApiError KnowledgeModelDetail)
    | GetParentPackageCompleted (Result ApiError PackageDetail)
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PutKnowledgeModelVersionCompleted (Result ApiError ())
