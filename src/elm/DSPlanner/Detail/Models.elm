module DSPlanner.Detail.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Common.Questionnaire.Models
import KMEditor.Common.Models.Entities exposing (Level)


type alias Model =
    { uuid : String
    , questionnaireModel : ActionResult Common.Questionnaire.Models.Model
    , levels : ActionResult (List Level)
    , savingQuestionnaire : ActionResult String
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaireModel = Loading
    , levels = Loading
    , savingQuestionnaire = Unset
    }
