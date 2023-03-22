module Wizard.Auth.Msgs exposing (Msg(..))

import Shared.Data.Token exposing (Token)
import Shared.Data.User exposing (User)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Routes exposing (Route)


type Msg
    = GetCurrentUserCompleted (Maybe String) (Result ApiError User)
    | Logout
    | LogoutTo Route
    | LogoutDone
    | GotToken Token (Maybe String)
    | UpdateUser User
