module Wizard.Pages.Projects.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Debouncer.Extra as Debounce exposing (Debouncer)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Components.Listing.Models as Listing
import Wizard.Pages.Projects.Common.CloneProjectModal.Models as CloneProjectModal
import Wizard.Pages.Projects.Common.DeleteProjectModal.Models as DeleteProjectModal
import Wizard.Pages.Projects.Index.Msgs exposing (Msg)
import Wizard.Pages.Projects.Routes exposing (indexRouteIsTemplateFilterId, indexRoutePackagesFilterId, indexRouteProjectTagsFilterId, indexRouteUsersFilterId)


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
    , kmPackagesFilterSearchValue : String
    , kmPackagesFilterSelectedPackages : ActionResult (Pagination KnowledgeModelPackageSuggestion)
    , kmPackagesFilterPackages : ActionResult (Pagination KnowledgeModelPackageSuggestion)
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
initialModel paginationQueryString mbIsTemplate mbUser mbUserOp mbProjectTags mbProjectTagsOp mbKmPackages mbKmPackagesOp mbOldModel =
    let
        values =
            [ ( indexRouteIsTemplateFilterId, mbIsTemplate )
            , ( indexRouteUsersFilterId, mbUser )
            , ( indexRouteProjectTagsFilterId, mbProjectTags )
            , ( indexRoutePackagesFilterId, mbKmPackages )
            ]

        operators =
            [ ( indexRouteUsersFilterId, mbUserOp )
            , ( indexRouteProjectTagsFilterId, mbProjectTagsOp )
            , ( indexRoutePackagesFilterId, mbKmPackagesOp )
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
    , kmPackagesFilterSearchValue = ""
    , kmPackagesFilterSelectedPackages = selectedValue indexRoutePackagesFilterId
    , kmPackagesFilterPackages = ActionResult.Loading
    }
