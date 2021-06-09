module Wizard.Projects.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Dict
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Utils exposing (dictFromMaybeList)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Projects.Common.CloneProjectModal.Models as CloneProjectModal
import Wizard.Projects.Common.DeleteProjectModal.Models as DeleteProjectModal
import Wizard.Projects.Routes exposing (indexRouteIsTemplateFilterId)


type alias Model =
    { questionnaires : Listing.Model Questionnaire
    , deletingMigration : ActionResult String
    , deleteModalModel : DeleteProjectModal.Model
    , cloneModalModel : CloneProjectModal.Model
    }


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel paginationQueryString mbIsTemplate =
    let
        filters =
            dictFromMaybeList [ ( indexRouteIsTemplateFilterId, mbIsTemplate ) ]
    in
    { questionnaires = Listing.initialModelWithFilters paginationQueryString filters
    , deletingMigration = Unset
    , deleteModalModel = DeleteProjectModal.initialModel
    , cloneModalModel = CloneProjectModal.initialModel
    }
