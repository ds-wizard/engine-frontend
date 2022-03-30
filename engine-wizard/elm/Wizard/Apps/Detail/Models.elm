module Wizard.Apps.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.AppDetail exposing (AppDetail)
import Shared.Data.Plan exposing (Plan)
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Apps.Common.AppEditForm exposing (AppEditForm)
import Wizard.Apps.Common.PlanForm exposing (PlanForm)


type alias Model =
    { uuid : Uuid
    , app : ActionResult AppDetail
    , editForm : Maybe (Form FormError AppEditForm)
    , savingApp : ActionResult String
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
    , app = ActionResult.Loading
    , editForm = Nothing
    , savingApp = Unset
    , addPlanForm = Nothing
    , addingPlan = Unset
    , editPlanForm = Nothing
    , editingPlan = Unset
    , deletePlan = Nothing
    , deletingPlan = Unset
    }
