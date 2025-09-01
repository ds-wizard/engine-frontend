module Wizard.KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)


type Msg
    = ChangePackageId String
    | Submit
    | PullPackageCompleted (Result ApiError ())
