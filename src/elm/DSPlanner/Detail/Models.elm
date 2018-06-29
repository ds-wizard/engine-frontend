module DSPlanner.Detail.Models exposing (..)

import Common.Questionnaire.Models
import Common.Types exposing (ActionResult(..))


type alias Model =
    { uuid : String
    , questionnaireModel : ActionResult Common.Questionnaire.Models.Model
    , savingQuestionnaire : ActionResult String
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaireModel = Loading
    , savingQuestionnaire = Unset
    }
