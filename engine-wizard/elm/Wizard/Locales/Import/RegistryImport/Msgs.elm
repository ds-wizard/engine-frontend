module Wizard.Locales.Import.RegistryImport.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)


type Msg
    = ChangeLocaleId String
    | Submit
    | PullLocaleCompleted (Result ApiError ())
