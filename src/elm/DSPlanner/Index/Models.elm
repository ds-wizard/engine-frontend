module DSPlanner.Index.Models exposing (..)

import Bootstrap.Dropdown as Dropdown
import Common.Types exposing (ActionResult(..))
import DSPlanner.Common.Models exposing (Questionnaire)


type alias Model =
    { questionnaires : ActionResult (List QuestionnaireRow)
    , questionnaireToBeDeleted : Maybe Questionnaire
    , deletingQuestionnaire : ActionResult String
    }


initialModel : Model
initialModel =
    { questionnaires = Loading
    , questionnaireToBeDeleted = Nothing
    , deletingQuestionnaire = Unset
    }


type alias QuestionnaireRow =
    { dropdownState : Dropdown.State
    , questionnaire : Questionnaire
    }


initQuestionnaireRow : Questionnaire -> QuestionnaireRow
initQuestionnaireRow =
    QuestionnaireRow Dropdown.initialState
