module Wizard.Pages.ProjectImporters.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.ProjectImporter exposing (ProjectImporter)
import Wizard.Components.Listing.Models as Listing


type alias Model =
    { questionnaireImporters : Listing.Model ProjectImporter
    , togglingEnabled : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { questionnaireImporters = Listing.initialModel paginationQueryString
    , togglingEnabled = Unset
    }
