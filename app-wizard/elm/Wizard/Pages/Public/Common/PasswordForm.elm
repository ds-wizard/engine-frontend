module Wizard.Pages.Public.Common.PasswordForm exposing
    ( PasswordForm
    , encode
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Form.Validate as V
import Wizard.Data.AppState exposing (AppState)


type alias PasswordForm =
    { password : String
    , passwordConfirmation : String
    }


initEmpty : AppState -> Form FormError PasswordForm
initEmpty appState =
    Form.initial [] (validation appState)


validation : AppState -> Validation FormError PasswordForm
validation appState =
    V.map2 PasswordForm
        (V.field "password" (V.password appState))
        (V.field "password" V.string |> V.confirmation "passwordConfirmation")


encode : PasswordForm -> E.Value
encode form =
    E.object
        [ ( "password", E.string form.password )
        ]
