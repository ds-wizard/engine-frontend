module KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import KnowledgeModels.Common.Package exposing (Package)


type Msg
    = ChangePackageId String
    | Submit
    | PullPackageCompleted (Result ApiError Package)
