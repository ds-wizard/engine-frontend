module Wizard.Projects.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Debouncer.Extra as Debounce exposing (Debouncer)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Api.Models.UserSuggestion exposing (UserSuggestion)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Projects.Common.CloneProjectModal.Models as CloneProjectModal
import Wizard.Projects.Common.DeleteProjectModal.Models as DeleteProjectModal
import Wizard.Projects.Index.Msgs exposing (Msg)
import Wizard.Projects.Routes exposing (indexRouteIsTemplateFilterId, indexRoutePackagesFilterId, indexRouteProjectTagsFilterId, indexRouteUsersFilterId)


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
    , packagesFilterSearchValue : String
    , packagesFilterSelectedPackages : ActionResult (Pagination PackageSuggestion)
    , packagesFilterPackages : ActionResult (Pagination PackageSuggestion)
    }


initialModel :
    PaginationQueryString
    -> Maybe String
    -> Maybe String
    -> Maybe FilterOperator
    -> Maybe String
    -> Maybe FilterOperator
    -> Maybe String
    -> Maybe FilterOperator
    -> Maybe Model
    -> Model
initialModel paginationQueryString mbIsTemplate mbUser mbUserOp mbProjectTags mbProjectTagsOp mbPackages mbPackagesOp mbOldModel =
    let
        values =
            [ ( indexRouteIsTemplateFilterId, mbIsTemplate )
            , ( indexRouteUsersFilterId, mbUser )
            , ( indexRouteProjectTagsFilterId, mbProjectTags )
            , ( indexRoutePackagesFilterId, mbPackages )
            ]

        operators =
            [ ( indexRouteUsersFilterId, mbUserOp )
            , ( indexRouteProjectTagsFilterId, mbProjectTagsOp )
            , ( indexRoutePackagesFilterId, mbPackagesOp )
            ]

        paginationQueryFilters =
            PaginationQueryFilters.create values operators

        selectedValue filterId =
            if PaginationQueryFilters.isFilterActive filterId paginationQueryFilters then
                ActionResult.Loading

            else
                ActionResult.Success Pagination.empty
    in
    { questionnaires = Listing.initialModelWithFiltersAndStates paginationQueryString paginationQueryFilters (Maybe.map .questionnaires mbOldModel)
    , deletingMigration = Unset
    , deleteModalModel = DeleteProjectModal.initialModel
    , cloneModalModel = CloneProjectModal.initialModel
    , debouncer = Debounce.toDebouncer <| Debounce.debounce 500
    , projectTagsExist = ActionResult.Loading
    , projectTagsFilterSearchValue = ""
    , projectTagsFilterTags = ActionResult.Loading
    , userFilterSearchValue = ""
    , userFilterSelectedUsers = selectedValue indexRouteUsersFilterId
    , userFilterUsers = ActionResult.Loading
    , packagesFilterSearchValue = ""
    , packagesFilterSelectedPackages = selectedValue indexRoutePackagesFilterId
    , packagesFilterPackages = ActionResult.Loading
    }
