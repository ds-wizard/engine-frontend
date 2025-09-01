module Wizard.Projects.Common.CloneProjectModal.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type alias Model =
    { questionnaireToBeDeleted : Maybe QuestionnaireDescriptor
    , cloningQuestionnaire : ActionResult String
    }


initialModel : Model
initialModel =
    { questionnaireToBeDeleted = Nothing
    , cloningQuestionnaire = Unset
    }
