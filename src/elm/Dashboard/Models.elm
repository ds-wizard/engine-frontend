module Dashboard.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import KMEditor.Common.Models.Entities exposing (Level)
import Questionnaires.Common.Questionnaire exposing (Questionnaire)


type alias Model =
    { levels : ActionResult (List Level)
    , questionnaires : ActionResult (List Questionnaire)
    }


initialModel : Model
initialModel =
    { levels = Loading
    , questionnaires = Loading
    }
