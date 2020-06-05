module Wizard.Questionnaires.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Common.Pagination.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)


type alias Model =
    { questionnaires : Listing.Model Questionnaire
    , questionnaireToBeDeleted : Maybe Questionnaire
    , deletingQuestionnaire : ActionResult String
    , deletingMigration : ActionResult String
    , cloningQuestionnaire : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { questionnaires = Listing.initialModel paginationQueryString
    , questionnaireToBeDeleted = Nothing
    , deletingQuestionnaire = Unset
    , deletingMigration = Unset
    , cloningQuestionnaire = Unset
    }
