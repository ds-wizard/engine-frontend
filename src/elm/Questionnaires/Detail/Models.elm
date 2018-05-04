module Questionnaires.Detail.Models exposing (..)

import Common.Types exposing (ActionResult(Loading))
import Questionnaires.Common.Models exposing (QuestionnaireDetail)


type alias Model =
    { questionnaire : ActionResult QuestionnaireDetail
    }


initialModel : Model
initialModel =
    { questionnaire = Loading
    }
