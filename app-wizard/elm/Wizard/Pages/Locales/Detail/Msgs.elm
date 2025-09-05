module Wizard.Pages.Locales.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.Components.FileDownloader as FileDownloader
import Common.Data.ApiError exposing (ApiError)
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
