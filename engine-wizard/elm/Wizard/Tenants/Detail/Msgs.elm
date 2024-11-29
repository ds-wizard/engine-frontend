module Wizard.Tenants.Detail.Msgs exposing (Msg(..))

import Form
import Shared.Data.TenantDetail exposing (TenantDetail)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetAppComplete (Result ApiError TenantDetail)
    | EditModalOpen
    | EditModalClose
    | EditModalFormMsg Form.Msg
    | PutAppComplete (Result ApiError ())
