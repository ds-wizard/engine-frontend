module Wizard.Tenants.Detail.Msgs exposing (Msg(..))

import Form
import Shared.Data.TenantDetail exposing (TenantDetail)
import Shared.Error.ApiError exposing (ApiError)


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
