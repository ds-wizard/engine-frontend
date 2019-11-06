module Wizard.Users.Index.Msgs exposing (Msg(..))

import Result exposing (Result)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Users.Common.User exposing (User)


type Msg
    = GetUsersCompleted (Result ApiError (List User))
    | ShowHideDeleteUser (Maybe User)
    | DeleteUser
    | DeleteUserCompleted (Result ApiError ())
