module DSPlanner.Index.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import DSPlanner.Common.Models exposing (Questionnaire)


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
