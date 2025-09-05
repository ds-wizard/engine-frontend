module Wizard.Pages.Locales.Import.RegistryImport.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)


type Msg
    = ChangeLocaleId String
    | Submit
    | PullLocaleCompleted (Result ApiError ())
