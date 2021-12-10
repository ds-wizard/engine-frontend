module Wizard.Projects.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Debouncer.Extra as Debounce exposing (Debouncer)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Utils exposing (dictFromMaybeList)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Projects.Common.CloneProjectModal.Models as CloneProjectModal
import Wizard.Projects.Common.DeleteProjectModal.Models as DeleteProjectModal
import Wizard.Projects.Index.Msgs exposing (Msg)
import Wizard.Projects.Routes exposing (indexRouteIsTemplateFilterId, indexRouteProjectTagsFilterId, indexRouteUsersFilterId)


type alias Model =
    { questionnaires : Listing.Model Questionnaire
    , deletingMigration : ActionResult String
    , deleteModalModel : DeleteProjectModal.Model
    , cloneModalModel : CloneProjectModal.Model
    , projectTagsFilterSearchValue : String
    , projectTagsFilterDebouncer : Debouncer Msg
    , projectTagsFilterTags : ActionResult (Pagination String)
    }


initialModel : PaginationQueryString -> Maybe String -> Maybe String -> Maybe String -> Model
initialModel paginationQueryString mbIsTemplate mbUser mbProjectTags =
    let
        filters =
            dictFromMaybeList
                [ ( indexRouteIsTemplateFilterId, mbIsTemplate )
                , ( indexRouteUsersFilterId, mbUser )
                , ( indexRouteProjectTagsFilterId, mbProjectTags )
                ]
    in
    { questionnaires = Listing.initialModelWithFilters paginationQueryString filters
    , deletingMigration = Unset
    , deleteModalModel = DeleteProjectModal.initialModel
    , cloneModalModel = CloneProjectModal.initialModel
    , projectTagsFilterSearchValue = ""
    , projectTagsFilterDebouncer = Debounce.toDebouncer <| Debounce.debounce 500
    , projectTagsFilterTags = ActionResult.Unset
    }
