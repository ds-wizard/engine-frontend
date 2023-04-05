module Wizard.Locales.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Shared.Data.LocaleDetail exposing (LocaleDetail)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.FileDownloader as FileDownloader


type Msg
    = GetLocaleCompleted (Result ApiError LocaleDetail)
    | DropdownMsg Dropdown.State
    | ShowDeleteDialog Bool
    | DeleteVersion
    | DeleteVersionCompleted (Result ApiError ())
    | SetDefault
    | SetDefaultCompleted
    | SetEnabled Bool
    | SetEnabledCompleted
    | ExportLocale LocaleDetail
    | FileDownloaderMsg FileDownloader.Msg
