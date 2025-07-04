module Wizard.Tenants.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Api.Models.TenantDetail exposing (TenantDetail)
import Wizard.Tenants.Common.TenantEditForm exposing (TenantEditForm)
import Wizard.Tenants.Common.TenantLimitsForm exposing (TenantLimitsForm)


type alias Model =
    { uuid : Uuid
    , tenant : ActionResult TenantDetail
    , editForm : Maybe (Form FormError TenantEditForm)
    , limitsForm : Maybe (Form FormError TenantLimitsForm)
    , savingTenant : ActionResult String
    }


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , tenant = ActionResult.Loading
    , editForm = Nothing
    , limitsForm = Nothing
    , savingTenant = Unset
    }
