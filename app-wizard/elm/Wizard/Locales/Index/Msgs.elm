module Wizard.Locales.Index.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.Locale exposing (Locale)
import Wizard.Common.Components.Listing.Msgs as Listing
import Wizard.Common.FileDownloader as FileDownloader


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
