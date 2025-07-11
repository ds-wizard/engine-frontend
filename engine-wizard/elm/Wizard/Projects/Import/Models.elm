module Wizard.Projects.Import.Models exposing
    ( Model
    , SidePanel(..)
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireImporter exposing (QuestionnaireImporter)
import Wizard.Api.Models.QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.Importer exposing (ImporterResult)


type alias Model =
    { uuid : Uuid
    , sidePanel : SidePanel
    , importerId : String
    , questionnaire : ActionResult QuestionnaireQuestionnaire
    , questionnaireModel : ActionResult Questionnaire.Model
    , questionnaireImporter : ActionResult QuestionnaireImporter
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
