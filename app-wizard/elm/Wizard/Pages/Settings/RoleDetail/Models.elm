module Wizard.Pages.Settings.RoleDetail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Api.Models.Role exposing (Role)
import Common.Api.Models.RolePermission exposing (RolePermission)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Uuid exposing (Uuid)
import Wizard.Pages.Settings.Common.Forms.RoleForm as RoleForm exposing (RoleForm)


type alias Model =
    { uuid : Uuid
    , role : ActionResult Role
    , form : Form FormError RoleForm
    , permissions : List RolePermission
    , savingRole : ActionResult ()
    }


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , role = ActionResult.Loading
    , form = RoleForm.initEmpty
    , permissions = []
    , savingRole = ActionResult.Unset
    }
