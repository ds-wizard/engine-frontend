module Wizard.Settings.Usage.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.Usage exposing (Usage)


type alias Model =
    { usage : ActionResult Usage }


initialModel : Model
initialModel =
    { usage = Loading }
