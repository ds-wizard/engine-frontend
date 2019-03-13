module Questionnaires.Index.Models exposing (Model, QuestionnaireRow, initQuestionnaireRow, initialModel)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Questionnaires.Common.Models exposing (Questionnaire)


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
