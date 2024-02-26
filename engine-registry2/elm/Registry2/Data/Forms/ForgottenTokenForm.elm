module Registry2.Data.Forms.ForgottenTokenForm exposing (ForgottenTokenForm, encode, init, validation)

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)


type alias ForgottenTokenForm =
    { email : String }


init : Form FormError ForgottenTokenForm
init =
    Form.initial [] validation


validation : Validation FormError ForgottenTokenForm
validation =
    V.succeed ForgottenTokenForm
        |> V.andMap (V.field "email" V.email)


encode : ForgottenTokenForm -> E.Value
encode form =
    E.object
        [ ( "type", E.string "ForgottenTokenActionKey" )
        , ( "email", E.string form.email )
        ]
