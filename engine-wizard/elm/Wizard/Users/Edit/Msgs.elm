module Wizard.Users.Edit.Msgs exposing (Msg(..))

import Form
import Shared.Data.User exposing (User)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Users.Edit.Models exposing (View)


type Msg
    = GetUserCompleted (Result ApiError User)
    | EditFormMsg Form.Msg
    | PutUserCompleted (Result ApiError User)
    | PasswordFormMsg Form.Msg
    | PutUserPasswordCompleted (Result ApiError ())
    | ChangeView View
