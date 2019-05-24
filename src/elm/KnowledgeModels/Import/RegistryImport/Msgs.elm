module KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)


type Msg
    = ChangePackageId String
    | Submit
    | PullPackageCompleted (Result ApiError ())
