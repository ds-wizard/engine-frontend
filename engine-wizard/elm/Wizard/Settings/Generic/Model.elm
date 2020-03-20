module Wizard.Settings.Generic.Model exposing (..)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)


type alias Model config form =
    { config : ActionResult config
    , savingConfig : ActionResult ()
    , form : Form CustomFormError form
    }


initialModel : Form CustomFormError form -> Model config form
initialModel form =
    { config = Loading
    , savingConfig = Unset
    , form = form
    }
