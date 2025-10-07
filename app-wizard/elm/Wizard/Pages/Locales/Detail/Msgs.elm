module Wizard.Pages.Locales.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.Api.ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Wizard.Api.Models.LocaleDetail exposing (LocaleDetail)


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
