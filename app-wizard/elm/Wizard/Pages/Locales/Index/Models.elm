module Wizard.Pages.Locales.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.Locale exposing (Locale)
import Wizard.Components.Listing.Models as Listing


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
