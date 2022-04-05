module Wizard.Apps.Detail.Msgs exposing (Msg(..))

import Form
import Shared.Data.AppDetail exposing (AppDetail)
import Shared.Data.Plan exposing (Plan)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetAppComplete (Result ApiError AppDetail)
    | EditModalOpen
    | EditModalClose
    | EditModalFormMsg Form.Msg
    | PutAppComplete (Result ApiError ())
    | AddPlanModalOpen
    | AddPlanModalClose
    | AddPlanModalFormMsg Form.Msg
    | PostPlanComplete (Result ApiError ())
    | EditPlanModalOpen Plan
    | EditPlanModalClose
    | EditPlanModalFormMsg Form.Msg
    | PutPlanComplete (Result ApiError ())
    | DeletePlanModalOpen Plan
    | DeletePlanModalClose
    | DeletePlanModalConfirm
    | DeletePlanComplete (Result ApiError ())
