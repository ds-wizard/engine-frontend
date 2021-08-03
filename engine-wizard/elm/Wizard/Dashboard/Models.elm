module Wizard.Dashboard.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.Questionnaire exposing (Questionnaire)


type alias Model =
    { questionnaires : ActionResult (List Questionnaire)
    }


initialModel : Model
initialModel =
    { questionnaires = Loading
    }
