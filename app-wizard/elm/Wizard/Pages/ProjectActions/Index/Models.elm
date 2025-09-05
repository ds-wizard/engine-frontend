module Wizard.Pages.ProjectActions.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.QuestionnaireAction exposing (QuestionnaireAction)
import Wizard.Components.Listing.Models as Listing


type alias Model =
    { questionnaireActions : Listing.Model QuestionnaireAction
    , togglingEnabled : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { questionnaireActions = Listing.initialModel paginationQueryString
    , togglingEnabled = Unset
    }
