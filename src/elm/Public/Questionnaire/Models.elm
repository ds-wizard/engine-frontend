module Public.Questionnaire.Models exposing (..)

import Common.Questionnaire.Models
import Common.Types exposing (ActionResult(Loading))
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias Model =
    { questionnaireModel : ActionResult Common.Questionnaire.Models.Model
    }


initialModel : Model
initialModel =
    { questionnaireModel = Loading
    }
