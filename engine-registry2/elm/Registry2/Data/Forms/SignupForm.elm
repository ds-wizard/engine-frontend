module Registry2.Data.Forms.SignupForm exposing (SignupForm, encode, init, validation)

import Form exposing (Form)
import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias SignupForm =
    { organizationId : String
    , name : String
    , email : String
    , description : String
    , accept : Bool
    }


init : Form FormError SignupForm
init =
    Form.initial [] validation


validation : Validation FormError SignupForm
validation =
    V.succeed SignupForm
        |> V.andMap (V.field "organizationId" V.organizationId)
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "description" V.string)
        |> V.andMap (V.field "accept" validateAcceptField)


validateAcceptField : Field -> Result (Error customError) Bool
validateAcceptField v =
    if Field.asBool v |> Maybe.withDefault False then
        Ok True

    else
        Err (Error.value Empty)


encode : SignupForm -> E.Value
encode form =
    E.object
        [ ( "organizationId", E.string form.organizationId )
        , ( "name", E.string form.name )
        , ( "email", E.string form.email )
        , ( "description", E.string form.description )
        ]
