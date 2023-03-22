module Wizard.Auth.Msgs exposing (Msg(..))

import Shared.Data.Token exposing (Token)
import Shared.Data.User exposing (User)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetCurrentUserCompleted (Maybe String) (Result ApiError User)
    | Logout
    | LogoutDone
    | GotToken Token (Maybe String)
    | UpdateUser User
