module Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Questionnaires.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type alias Model =
    { questionnaireToBeDeleted : Maybe QuestionnaireDescriptor
    , deletingQuestionnaire : ActionResult String
    }


initialModel : Model
initialModel =
    { questionnaireToBeDeleted = Nothing
    , deletingQuestionnaire = Unset
    }
