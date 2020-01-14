module Wizard.Dashboard.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)


type alias Model =
    { levels : ActionResult (List Level)
    , questionnaires : ActionResult (List Questionnaire)
    }


initialModel : Model
initialModel =
    { levels = Loading
    , questionnaires = Loading
    }
