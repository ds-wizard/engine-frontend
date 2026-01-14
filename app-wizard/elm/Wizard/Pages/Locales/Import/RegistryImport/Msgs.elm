module Wizard.Pages.Locales.Import.RegistryImport.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.LocaleInfo exposing (LocaleInfo)


type Msg
    = ChangeLocaleId String
    | Submit
    | PullLocaleCompleted (Result ApiError LocaleInfo)
