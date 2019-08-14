module Users.Edit.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import Users.Common.User exposing (User)
import Users.Edit.Models exposing (View)


type Msg
    = GetUserCompleted (Result ApiError User)
    | EditFormMsg Form.Msg
    | PutUserCompleted (Result ApiError ())
    | PasswordFormMsg Form.Msg
    | PutUserPasswordCompleted (Result ApiError ())
    | ChangeView View
