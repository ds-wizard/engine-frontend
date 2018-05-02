module Questionnaires.Index.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import Questionnaires.Common.Models exposing (Questionnaire)


type alias Model =
    { questionnaires : ActionResult (List Questionnaire)
    }


initialModel : Model
initialModel =
    { questionnaires = Loading
    }
