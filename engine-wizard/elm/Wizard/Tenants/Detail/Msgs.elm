module Wizard.Tenants.Detail.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.TenantDetail exposing (TenantDetail)


type Msg
    = GetTenantComplete (Result ApiError TenantDetail)
    | EditModalOpen
    | EditModalClose
    | EditModalFormMsg Form.Msg
    | PutTenantComplete (Result ApiError ())
    | EditLimitsModalOpen
    | EditLimitsModalClose
    | EditLimitsModalFormMsg Form.Msg
    | PutTenantLimitsComplete (Result ApiError ())
