module Wizard.Users.Common.UserPasswordForm exposing (UserPasswordForm, encode, init, validation)

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Form.Validate as V
import Wizard.Common.AppState exposing (AppState)


type alias UserPasswordForm =
    { password : String
    , passwordConfirmation : String
    }


init : AppState -> Form FormError UserPasswordForm
init appState =
    Form.initial [] (validation appState)


validation : AppState -> Validation FormError UserPasswordForm
validation appState =
    V.map2 UserPasswordForm
        (V.field "password" (V.password appState))
        (V.field "password" V.string |> V.confirmation "passwordConfirmation")


encode : UserPasswordForm -> E.Value
encode form =
    E.object
        [ ( "password", E.string form.password )
        ]
