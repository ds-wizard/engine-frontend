module Public.Questionnaire.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Common.Questionnaire.Models


type alias Model =
    { questionnaireModel : ActionResult Common.Questionnaire.Models.Model
    }


initialModel : Model
initialModel =
    { questionnaireModel = Loading
    }
