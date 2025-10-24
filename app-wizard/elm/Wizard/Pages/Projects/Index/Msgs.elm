module Wizard.Pages.Projects.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Debouncer.Extra as Debouncer
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Components.Listing.Msgs as Listing
import Wizard.Pages.Projects.Common.CloneProjectModal.Msgs as CloneProjectModal
import Wizard.Pages.Projects.Common.DeleteProjectModal.Msgs as DeleteProjectModal


type Msg
    = DeleteQuestionnaireMigration Uuid
    | DeleteQuestionnaireMigrationCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Questionnaire)
    | ListingFilterAddSelectedPackage KnowledgeModelPackageSuggestion (Listing.Msg Questionnaire)
    | ListingFilterAddSelectedUser UserSuggestion (Listing.Msg Questionnaire)
    | DeleteQuestionnaireModalMsg DeleteProjectModal.Msg
    | CloneQuestionnaireModalMsg CloneProjectModal.Msg
    | ProjectTagsFilterInput String
    | ProjectTagsFilterSearch String
    | ProjectTagsFilterSearchComplete String (Result ApiError (Pagination String))
    | UsersFilterGetValuesComplete (Result ApiError (Pagination UserSuggestion))
    | UsersFilterInput String
    | UsersFilterSearch String
    | UsersFilterSearchComplete (Result ApiError (Pagination UserSuggestion))
    | PackagesFilterGetValuesComplete (Result ApiError (Pagination KnowledgeModelPackageSuggestion))
    | PackagesFilterInput String
    | PackagesFilterSearch String
    | PackagesFilterSearchComplete (Result ApiError (Pagination KnowledgeModelPackageSuggestion))
    | DebouncerMsg (Debouncer.Msg Msg)
