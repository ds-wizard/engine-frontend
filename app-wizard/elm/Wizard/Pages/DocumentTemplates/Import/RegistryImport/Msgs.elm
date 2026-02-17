module Wizard.Pages.DocumentTemplates.Import.RegistryImport.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.UuidResponse exposing (UuidResponse)


type Msg
    = ChangeTemplateId String
    | Submit
    | PullTemplateCompleted (Result ApiError UuidResponse)
