module Wizard.Pages.ProjectActions.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.ProjectAction exposing (ProjectAction)
import Wizard.Components.Listing.Models as Listing


type alias Model =
    { questionnaireActions : Listing.Model ProjectAction
    , togglingEnabled : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { questionnaireActions = Listing.initialModel paginationQueryString
    , togglingEnabled = Unset
    }
