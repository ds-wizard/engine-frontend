module Wizard.Pages.Tenants.Detail.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Form
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
