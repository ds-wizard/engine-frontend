module Users.Common.UserPasswordForm exposing (UserPasswordForm, encode, init, validation)

import Common.Form exposing (CustomFormError)
import Common.Form.Validate exposing (validateConfirmation)
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E


type alias UserPasswordForm =
    { password : String
    , passwordConfirmation : String
    }


init : Form CustomFormError UserPasswordForm
init =
    Form.initial [] validation


validation : Validation CustomFormError UserPasswordForm
validation =
    Validate.map2 UserPasswordForm
        (Validate.field "password" Validate.string)
        (Validate.field "password" Validate.string |> validateConfirmation "passwordConfirmation")


encode : UserPasswordForm -> E.Value
encode form =
    E.object
        [ ( "password", E.string form.password )
        ]
