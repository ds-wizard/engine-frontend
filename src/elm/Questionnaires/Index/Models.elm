module Questionnaires.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Questionnaires.Common.Models exposing (Questionnaire)


type alias Model =
    { questionnaires : ActionResult (List Questionnaire)
    , questionnaireToBeDeleted : Maybe Questionnaire
    , deletingQuestionnaire : ActionResult String
    , questionnaireToBeExported : Maybe Questionnaire
    }


initialModel : Model
initialModel =
    { questionnaires = Loading
    , questionnaireToBeDeleted = Nothing
    , deletingQuestionnaire = Unset
    , questionnaireToBeExported = Nothing
    }
