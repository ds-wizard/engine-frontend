module Wizard.Projects.Import.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Uuid exposing (Uuid)
import Wizard.Common.Components.Questionnaire.Importer exposing (ImporterResult)


type alias Model =
    { uuid : Uuid
    , importerId : String
    , questionnaire : ActionResult QuestionnaireDetail
    , questionnaireImporter : ActionResult QuestionnaireImporter
    , importResult : Maybe ImporterResult
    , importing : ActionResult ()
    }


initialModel : Uuid -> String -> Model
initialModel uuid importerId =
    { uuid = uuid
    , importerId = importerId
    , questionnaire = Loading
    , questionnaireImporter = Loading
    , importResult = Nothing
    , importing = Unset
    }
