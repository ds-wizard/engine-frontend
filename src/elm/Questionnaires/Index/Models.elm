module Questionnaires.Index.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import Questionnaires.Common.Models exposing (Questionnaire)


type alias Model =
    { questionnaires : ActionResult (List Questionnaire)
    , questionnaireToBeDeleted : Maybe Questionnaire
    , deletingQuestionnaire : ActionResult String
    }


initialModel : Model
initialModel =
    { questionnaires = Loading
    , questionnaireToBeDeleted = Nothing
    , deletingQuestionnaire = Unset
    }
