module Wizard.Users.Edit.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Users.Common.User exposing (User)
import Wizard.Users.Edit.Models exposing (View)


type Msg
    = GetUserCompleted (Result ApiError User)
    | EditFormMsg Form.Msg
    | PutUserCompleted (Result ApiError ())
    | PasswordFormMsg Form.Msg
    | PutUserPasswordCompleted (Result ApiError ())
    | ChangeView View
