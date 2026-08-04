module Wizard.Pages.Settings.Roles.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Role exposing (Role)


type Msg
    = GetRolesCompleted (Result ApiError (Pagination Role))
    | ShowHideDeleteRole (Maybe Role)
    | DeleteRole
    | DeleteRoleCompleted (Result ApiError ())
