module Wizard.Questionnaires.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing as Listing
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)


type alias Model =
    { questionnaires : ActionResult (Listing.Model Questionnaire)
    , questionnaireToBeDeleted : Maybe Questionnaire
    , deletingQuestionnaire : ActionResult String
    , deletingMigration : ActionResult String
    , cloningQuestionnaire : ActionResult String
    }


initialModel : Model
initialModel =
    { questionnaires = Loading
    , questionnaireToBeDeleted = Nothing
    , deletingQuestionnaire = Unset
    , deletingMigration = Unset
    , cloningQuestionnaire = Unset
    }
