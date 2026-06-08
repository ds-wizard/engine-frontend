module Wizard.Pages.Settings.RoleCreate.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Role exposing (Role)
import Common.Api.Models.RolePermission exposing (RolePermission)
import Form


type Msg
    = FormMsg Form.Msg
    | AddPermission RolePermission
    | RemovePermission RolePermission
    | Cancel
    | PostRoleComplete (Result ApiError Role)
