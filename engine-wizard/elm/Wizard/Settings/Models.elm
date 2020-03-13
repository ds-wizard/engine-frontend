module Wizard.Settings.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.ConfigForm as ConfigForm exposing (ConfigForm)
import Wizard.Settings.Common.EditableConfig exposing (EditableConfig)


type alias Model =
    { config : ActionResult EditableConfig
    , savingConfig : ActionResult String
    , form : Form CustomFormError ConfigForm
    }


initialModel : Model
initialModel =
    { config = Loading
    , savingConfig = Unset
    , form = ConfigForm.initEmpty
    }
