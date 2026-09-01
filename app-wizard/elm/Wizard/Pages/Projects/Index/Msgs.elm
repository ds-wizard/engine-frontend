module Wizard.Pages.Projects.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Debouncer.Extra as Debouncer
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.Project exposing (Project)
import Wizard.Api.Models.UserGroupSuggestion exposing (UserGroupSuggestion)
import Wizard.Components.Listing.Msgs as Listing
import Wizard.Pages.Projects.Common.CloneProjectModal.Msgs as CloneProjectModal
import Wizard.Pages.Projects.Common.DeleteProjectModal.Msgs as DeleteProjectModal


type Msg
    = ListingMsg (Listing.Msg Project)
    | ListingFilterAddSelectedPackage KnowledgeModelPackageSuggestion (Listing.Msg Project)
    | ListingFilterAddSelectedUser UserSuggestion (Listing.Msg Project)
    | ListingFilterAddSelectedUserGroup UserGroupSuggestion (Listing.Msg Project)
    | DeleteQuestionnaireModalMsg DeleteProjectModal.Msg
    | CloneQuestionnaireModalMsg CloneProjectModal.Msg
    | ProjectTagsFilterInput String
    | ProjectTagsFilterSearch String
    | ProjectTagsFilterSearchComplete String (Result ApiError (Pagination String))
    | UsersFilterGetValuesComplete (Result ApiError (Pagination UserSuggestion))
    | UsersFilterInput String
    | UsersFilterSearch String
    | UsersFilterSearchComplete (Result ApiError (Pagination UserSuggestion))
    | UserGroupsFilterGetValuesComplete (Result ApiError (Pagination UserGroupSuggestion))
    | UserGroupsFilterInput String
    | UserGroupsFilterSearch String
    | UserGroupsFilterSearchComplete (Result ApiError (Pagination UserGroupSuggestion))
    | PackagesFilterGetValuesComplete (Result ApiError (Pagination KnowledgeModelPackageSuggestion))
    | PackagesFilterInput String
    | PackagesFilterSearch String
    | PackagesFilterSearchComplete (Result ApiError (Pagination KnowledgeModelPackageSuggestion))
    | DebouncerMsg (Debouncer.Msg Msg)
