module Wizard.Pages.KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.UuidResponse exposing (UuidResponse)


type Msg
    = ChangePackageId String
    | Submit
    | PullPackageCompleted (Result ApiError UuidResponse)
