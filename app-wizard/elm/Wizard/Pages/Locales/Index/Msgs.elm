module Wizard.Pages.Locales.Index.Msgs exposing (Msg(..))

import Common.Components.FileDownloader as FileDownloader
import Common.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.Locale exposing (Locale)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeleteLocale (Maybe Locale)
    | DeleteLocale
    | DeleteLocaleCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Locale)
    | SetEnabled Bool Locale
    | SetDefault Locale
    | ChangeLocaleCompleted (Result ApiError ())
    | ExportLocale Locale
    | FileDownloaderMsg FileDownloader.Msg
