module Wizard.Pages.Settings.RoleDetail.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Role exposing (Role)
import Common.Api.Models.RolePermission exposing (RolePermission)
import Form


type Msg
    = GetRoleComplete (Result ApiError Role)
    | PutRoleComplete (Result ApiError ())
    | FormMsg Form.Msg
    | AddPermission RolePermission
    | RemovePermission RolePermission
