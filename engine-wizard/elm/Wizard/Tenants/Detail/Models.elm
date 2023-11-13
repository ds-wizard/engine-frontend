module Wizard.Tenants.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.Plan exposing (Plan)
import Shared.Data.TenantDetail exposing (TenantDetail)
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Tenants.Common.PlanForm exposing (PlanForm)
import Wizard.Tenants.Common.TenantEditForm exposing (TenantEditForm)


type alias Model =
    { uuid : Uuid
    , tenant : ActionResult TenantDetail
    , editForm : Maybe (Form FormError TenantEditForm)
    , savingTenant : ActionResult String
    , addPlanForm : Maybe (Form FormError PlanForm)
    , addingPlan : ActionResult String
    , editPlanForm : Maybe ( Uuid, Form FormError PlanForm )
    , editingPlan : ActionResult String
    , deletePlan : Maybe Plan
    , deletingPlan : ActionResult String
    }


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , tenant = ActionResult.Loading
    , editForm = Nothing
    , savingTenant = Unset
    , addPlanForm = Nothing
    , addingPlan = Unset
    , editPlanForm = Nothing
    , editingPlan = Unset
    , deletePlan = Nothing
    , deletingPlan = Unset
    }
