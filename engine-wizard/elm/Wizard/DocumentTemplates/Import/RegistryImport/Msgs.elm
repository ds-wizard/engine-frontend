module Wizard.DocumentTemplates.Import.RegistryImport.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)


type Msg
    = ChangeTemplateId String
    | Submit
    | PullTemplateCompleted (Result ApiError ())
