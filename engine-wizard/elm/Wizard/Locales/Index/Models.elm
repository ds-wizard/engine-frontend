module Wizard.Locales.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.Locale exposing (Locale)
import Wizard.Common.Components.Listing.Models as Listing


type alias Model =
    { locales : Listing.Model Locale
    , localeToBeDeleted : Maybe Locale
    , deletingLocale : ActionResult String
    , changingLocale : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { locales = Listing.initialModel paginationQueryString
    , localeToBeDeleted = Nothing
    , deletingLocale = ActionResult.Unset
    , changingLocale = ActionResult.Unset
    }
