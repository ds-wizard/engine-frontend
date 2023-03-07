module Wizard.Locales.Index.Msgs exposing (Msg(..))

import Shared.Data.Locale exposing (Locale)
import Shared.Error.ApiError exposing (ApiError)
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
