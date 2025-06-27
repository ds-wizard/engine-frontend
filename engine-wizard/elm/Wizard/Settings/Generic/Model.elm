module Wizard.Settings.Generic.Model exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Form.FormError exposing (FormError)
import Wizard.Api.Models.EditableConfig exposing (EditableConfig)


type alias Model form =
    { config : ActionResult EditableConfig
    , savingConfig : ActionResult ()
    , form : Form FormError form
    , formRemoved : Bool
    }


initialModel : Form FormError form -> Model form
initialModel form =
    { config = Loading
    , savingConfig = Unset
    , form = form
    , formRemoved = False
    }
