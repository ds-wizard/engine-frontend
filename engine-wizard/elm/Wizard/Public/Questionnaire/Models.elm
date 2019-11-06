module Wizard.Public.Questionnaire.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Questionnaire.Models


type alias Model =
    { questionnaireModel : ActionResult Wizard.Common.Questionnaire.Models.Model
    }


initialModel : Model
initialModel =
    { questionnaireModel = Loading
    }
