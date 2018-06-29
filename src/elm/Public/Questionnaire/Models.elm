module Public.Questionnaire.Models exposing (..)

import Common.Questionnaire.Models
import Common.Types exposing (ActionResult(Loading))


type alias Model =
    { questionnaireModel : ActionResult Common.Questionnaire.Models.Model
    }


initialModel : Model
initialModel =
    { questionnaireModel = Loading
    }
