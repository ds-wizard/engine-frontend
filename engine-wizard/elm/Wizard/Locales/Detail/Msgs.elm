module Wizard.Locales.Detail.Msgs exposing (Msg(..))

import Shared.Data.LocaleDetail exposing (LocaleDetail)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetLocaleCompleted (Result ApiError LocaleDetail)
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
    | ExportLocale LocaleDetail
    | SetDefault
    | SetDefaultCompleted
    | SetEnabled Bool
    | SetEnabledCompleted
