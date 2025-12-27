module Wizard.Pages.Projects.ImportLegacy.Models exposing
    ( Model
    , SidePanel(..)
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectImporter exposing (ProjectImporter)
import Wizard.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.Questionnaire.Importer exposing (ImporterResult)


type alias Model =
    { uuid : Uuid
    , sidePanel : SidePanel
    , importerId : String
    , questionnaire : ActionResult ProjectQuestionnaire
    , questionnaireModel : ActionResult Questionnaire.Model
    , questionnaireImporter : ActionResult ProjectImporter
    , knowledgeModelString : ActionResult String
    , importResult : Maybe ImporterResult
    , importing : ActionResult ()
    }


type SidePanel
    = ChangesSidePanel
    | ErrorsSidePanel


initialModel : Uuid -> String -> Model
initialModel uuid importerId =
    { uuid = uuid
    , sidePanel = ChangesSidePanel
    , importerId = importerId
    , questionnaire = Loading
    , questionnaireModel = Loading
    , questionnaireImporter = Loading
    , knowledgeModelString = Loading
    , importResult = Nothing
    , importing = Unset
    }
