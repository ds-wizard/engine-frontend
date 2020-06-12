module Wizard.Questionnaires.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Common.Pagination.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Models as DeleteQuestionnaireModal
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)


type alias Model =
    { questionnaires : Listing.Model Questionnaire
    , deletingMigration : ActionResult String
    , cloningQuestionnaire : ActionResult String
    , deleteModalModel : DeleteQuestionnaireModal.Model
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { questionnaires = Listing.initialModel paginationQueryString
    , deletingMigration = Unset
    , cloningQuestionnaire = Unset
    , deleteModalModel = DeleteQuestionnaireModal.initialModel
    }
