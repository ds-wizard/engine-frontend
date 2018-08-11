module DSPlanner.Detail.Models exposing (..)

import Common.Questionnaire.Models
import Common.Types exposing (ActionResult(..))
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
