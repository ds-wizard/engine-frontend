module Wizard.Pages.KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)


type Msg
    = ChangePackageId String
    | Submit
    | PullPackageCompleted (Result ApiError ())
