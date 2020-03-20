module Wizard.Settings.Client.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.ClientConfigForm as ClientConfigForm exposing (ClientConfigForm)
import Wizard.Settings.Common.EditableClientConfig exposing (EditableClientConfig)


type alias Model =
    { config : ActionResult EditableClientConfig
    , savingConfig : ActionResult String
    , form : Form CustomFormError ClientConfigForm
    }


initialModel : Model
initialModel =
    { config = Loading
    , savingConfig = Unset
    , form = ClientConfigForm.initEmpty
    }
