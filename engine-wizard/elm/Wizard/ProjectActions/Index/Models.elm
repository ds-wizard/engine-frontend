module Wizard.ProjectActions.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.QuestionnaireAction exposing (QuestionnaireAction)
import Wizard.Common.Components.Listing.Models as Listing


type alias Model =
    { questionnaireActions : Listing.Model QuestionnaireAction
    , togglingEnabled : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { questionnaireActions = Listing.initialModel paginationQueryString
    , togglingEnabled = Unset
    }
