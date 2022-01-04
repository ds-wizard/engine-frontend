module Wizard.Projects.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Debouncer.Extra as Debounce exposing (Debouncer)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.UserSuggestion exposing (UserSuggestion)
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
    , debouncer : Debouncer Msg
    , projectTagsExist : ActionResult Bool
    , projectTagsFilterSearchValue : String
    , projectTagsFilterTags : ActionResult (Pagination String)
    , userFilterSearchValue : String
    , userFilterSelectedUsers : ActionResult (Pagination UserSuggestion)
    , userFilterUsers : ActionResult (Pagination UserSuggestion)
    }


initialModel :
    PaginationQueryString
    -> Maybe String
    -> Maybe String
    -> Maybe FilterOperator
    -> Maybe String
    -> Maybe FilterOperator
    -> Model
initialModel paginationQueryString mbIsTemplate mbUser mbUserOp mbProjectTags mbProjectTagsOp =
    let
        values =
            [ ( indexRouteIsTemplateFilterId, mbIsTemplate )
            , ( indexRouteUsersFilterId, mbUser )
            , ( indexRouteProjectTagsFilterId, mbProjectTags )
            ]

        operators =
            [ ( indexRouteUsersFilterId, mbUserOp )
            , ( indexRouteProjectTagsFilterId, mbProjectTagsOp )
            ]

        paginationQueryFilters =
            PaginationQueryFilters.create values operators
    in
    { questionnaires = Listing.initialModelWithFilters paginationQueryString paginationQueryFilters
    , deletingMigration = Unset
    , deleteModalModel = DeleteProjectModal.initialModel
    , cloneModalModel = CloneProjectModal.initialModel
    , debouncer = Debounce.toDebouncer <| Debounce.debounce 500
    , projectTagsExist = ActionResult.Loading
    , projectTagsFilterSearchValue = ""
    , projectTagsFilterTags = ActionResult.Loading
    , userFilterSearchValue = ""
    , userFilterSelectedUsers = ActionResult.Loading
    , userFilterUsers = ActionResult.Loading
    }
