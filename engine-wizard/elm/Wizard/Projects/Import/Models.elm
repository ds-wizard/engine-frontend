module Wizard.Projects.Import.Models exposing
    ( Model
    , SidePanel(..)
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Uuid exposing (Uuid)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.Importer exposing (ImporterResult)


type alias Model =
    { uuid : Uuid
    , sidePanel : SidePanel
    , importerId : String
    , questionnaire : ActionResult QuestionnaireDetail
    , questionnaireModel : ActionResult Questionnaire.Model
    , questionnaireImporter : ActionResult QuestionnaireImporter
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
    , importResult = Nothing
    , importing = Unset
    }
