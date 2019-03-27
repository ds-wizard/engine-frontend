module Users.Index.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Result exposing (Result)
import Users.Common.Models exposing (User)


type Msg
    = GetUsersCompleted (Result ApiError (List User))
    | ShowHideDeleteUser (Maybe User)
    | DeleteUser
    | DeleteUserCompleted (Result ApiError ())
