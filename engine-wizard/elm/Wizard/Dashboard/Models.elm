module Wizard.Dashboard.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.Questionnaire exposing (Questionnaire)


type alias Model =
    { levels : ActionResult (List Level)
    , questionnaires : ActionResult (List Questionnaire)
    }


initialModel : Model
initialModel =
    { levels = Loading
    , questionnaires = Loading
    }
