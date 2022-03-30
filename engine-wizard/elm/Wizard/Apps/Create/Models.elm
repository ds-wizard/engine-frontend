module Wizard.Apps.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Form.FormError exposing (FormError)
import Wizard.Apps.Common.AppCreateForm as AppCreateForm exposing (AppCreateForm)


type alias Model =
    { savingApp : ActionResult String
    , form : Form FormError AppCreateForm
    }


initialModel : Model
initialModel =
    { savingApp = Unset
    , form = AppCreateForm.init
    }
