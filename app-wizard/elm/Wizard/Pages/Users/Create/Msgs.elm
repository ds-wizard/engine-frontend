module Wizard.Pages.Users.Create.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Role exposing (Role)
import Form


type Msg
    = GetRolesCompleted (Result ApiError (Pagination Role))
    | Cancel
    | FormMsg Form.Msg
    | PostUserCompleted (Result ApiError ())
