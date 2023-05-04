module Wizard.Projects.Index.Msgs exposing (Msg(..))

import Debouncer.Extra as Debouncer
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Shared.Error.ApiError exposing (ApiError)
import Uuid exposing (Uuid)
import Wizard.Common.Components.Listing.Msgs as Listing
import Wizard.Projects.Common.CloneProjectModal.Msgs as CloneProjectModal
import Wizard.Projects.Common.DeleteProjectModal.Msgs as DeleteProjectModal


type Msg
    = DeleteQuestionnaireMigration Uuid
    | DeleteQuestionnaireMigrationCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Questionnaire)
    | ListingFilterAddSelectedPackage PackageSuggestion (Listing.Msg Questionnaire)
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
    | PackagesFilterGetValuesComplete (Result ApiError (Pagination PackageSuggestion))
    | PackagesFilterInput String
    | PackagesFilterSearch String
    | PackagesFilterSearchComplete (Result ApiError (Pagination PackageSuggestion))
    | DebouncerMsg (Debouncer.Msg Msg)
    | NoOp
