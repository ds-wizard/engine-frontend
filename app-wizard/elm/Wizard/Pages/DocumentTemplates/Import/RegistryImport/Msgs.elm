module Wizard.Pages.DocumentTemplates.Import.RegistryImport.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)


type Msg
    = ChangeTemplateId String
    | Submit
    | PullTemplateCompleted (Result ApiError ())
