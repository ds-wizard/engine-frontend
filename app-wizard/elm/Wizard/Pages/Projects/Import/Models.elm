module Wizard.Pages.Projects.Import.Models exposing
    ( Model
    , SidePanel(..)
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.Questionnaire.Importer exposing (ImporterResult)


type alias Model =
    { uuid : Uuid
    , sidePanel : SidePanel
    , importerUrl : String
    , project : ActionResult ProjectQuestionnaire
    , questionnaireModel : ActionResult Questionnaire.Model
    , knowledgeModelString : ActionResult String
    , importResult : Maybe ImporterResult
    , importing : ActionResult ()
    }


type SidePanel
    = ChangesSidePanel
    | ErrorsSidePanel


initialModel : Uuid -> String -> Model
initialModel uuid importerUrl =
    { uuid = uuid
    , sidePanel = ChangesSidePanel
    , importerUrl = importerUrl
    , project = Loading
    , questionnaireModel = Loading
    , knowledgeModelString = Loading
    , importResult = Nothing
    , importing = Unset
    }
