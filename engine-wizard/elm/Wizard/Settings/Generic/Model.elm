module Wizard.Settings.Generic.Model exposing (..)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.EditableConfig exposing (EditableConfig)


type alias Model form =
    { config : ActionResult EditableConfig
    , savingConfig : ActionResult ()
    , form : Form CustomFormError form
    }


initialModel : Form CustomFormError form -> Model form
initialModel form =
    { config = Loading
    , savingConfig = Unset
    , form = form
    }
