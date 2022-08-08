module Wizard.ProjectImporters.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Wizard.Common.Components.Listing.Models as Listing


type alias Model =
    { questionnaireImporters : Listing.Model QuestionnaireImporter
    , togglingEnabled : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { questionnaireImporters = Listing.initialModel paginationQueryString
    , togglingEnabled = Unset
    }
