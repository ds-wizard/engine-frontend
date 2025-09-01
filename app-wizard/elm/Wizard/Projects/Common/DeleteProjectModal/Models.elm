module Wizard.Projects.Common.DeleteProjectModal.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type alias Model =
    { questionnaireToBeDeleted : Maybe QuestionnaireDescriptor
    , deletingQuestionnaire : ActionResult String
    }


initialModel : Model
initialModel =
    { questionnaireToBeDeleted = Nothing
    , deletingQuestionnaire = Unset
    }
