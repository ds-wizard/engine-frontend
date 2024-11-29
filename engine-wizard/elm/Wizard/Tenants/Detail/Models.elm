module Wizard.Tenants.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.TenantDetail exposing (TenantDetail)
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Tenants.Common.TenantEditForm exposing (TenantEditForm)


type alias Model =
    { uuid : Uuid
    , tenant : ActionResult TenantDetail
    , editForm : Maybe (Form FormError TenantEditForm)
    , savingTenant : ActionResult String
    }


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , tenant = ActionResult.Loading
    , editForm = Nothing
    , savingTenant = Unset
    }
