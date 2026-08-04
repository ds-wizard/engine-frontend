module Wizard.Pages.Settings.RoleCreate.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Api.Models.RolePermission exposing (RolePermission)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Pages.Settings.Common.Forms.RoleForm as RoleForm exposing (RoleForm)


type alias Model =
    { form : Form FormError RoleForm
    , permissions : List RolePermission
    , savingForm : ActionResult ()
    }


initialModel : Model
initialModel =
    { form = RoleForm.initEmpty
    , permissions = []
    , savingForm = ActionResult.Unset
    }
