module Wizard.Settings.Generic.Model exposing (..)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.EditableConfig exposing (EditableConfig)
import Shared.Form.FormError exposing (FormError)


type alias Model form =
    { config : ActionResult EditableConfig
    , savingConfig : ActionResult ()
    , form : Form FormError form
    }


initialModel : Form FormError form -> Model form
initialModel form =
    { config = Loading
    , savingConfig = Unset
    , form = form
    }
