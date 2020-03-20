module Wizard.Settings.Info.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.EditableInfoConfig exposing (EditableInfoConfig)
import Wizard.Settings.Common.InfoConfigForm as InfoConfigForm exposing (InfoConfigForm)


type alias Model =
    { config : ActionResult EditableInfoConfig
    , savingConfig : ActionResult String
    , form : Form CustomFormError InfoConfigForm
    }


initialModel : Model
initialModel =
    { config = Loading
    , savingConfig = Unset
    , form = InfoConfigForm.initEmpty
    }
