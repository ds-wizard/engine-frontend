module Wizard.Settings.Plans.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.Plan exposing (Plan)


type alias Model =
    { plans : ActionResult (List Plan) }


initialModel : Model
initialModel =
    { plans = Loading }
