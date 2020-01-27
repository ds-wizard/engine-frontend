module Wizard.Questionnaires.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing as Listing
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Index.ExportModal.Models as ExportModal


type alias Model =
    { questionnaires : ActionResult (Listing.Model Questionnaire)
    , questionnaireToBeDeleted : Maybe Questionnaire
    , deletingQuestionnaire : ActionResult String
    , exportModalModel : ExportModal.Model
    , deletingMigration : ActionResult String
    , cloningQuestionnaire : ActionResult String
    }


initialModel : Model
initialModel =
    { questionnaires = Loading
    , questionnaireToBeDeleted = Nothing
    , deletingQuestionnaire = Unset
    , exportModalModel = ExportModal.initialModel
    , deletingMigration = Unset
    , cloningQuestionnaire = Unset
    }
