module Wizard.Tenants.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Tenants.Common.TenantCreateForm as AppCreateForm exposing (TenantCreateForm)


type alias Model =
    { savingTenant : ActionResult String
    , form : Form FormError TenantCreateForm
    }


initialModel : Model
initialModel =
    { savingTenant = Unset
    , form = AppCreateForm.init
    }
