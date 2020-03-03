module Wizard.Public.Common.SignupForm exposing
    ( SignupForm
    , encode
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate exposing (..)


type alias SignupForm =
    { email : String
    , firstName : String
    , lastName : String
    , password : String
    , passwordConfirmation : String
    , accept : Bool
    }


initEmpty : Form CustomFormError SignupForm
initEmpty =
    Form.initial [] validation


validation : Validation CustomFormError SignupForm
validation =
    Validate.map6 SignupForm
        (Validate.field "email" Validate.email)
        (Validate.field "firstName" Validate.string)
        (Validate.field "lastName" Validate.string)
        (Validate.field "password" Validate.string)
        (Validate.field "password" Validate.string |> validateConfirmation "passwordConfirmation")
        (Validate.field "accept" validateAcceptField)


validateAcceptField : Field -> Result (Error customError) Bool
validateAcceptField v =
    if Field.asBool v |> Maybe.withDefault False then
        Ok True

    else
        Err (Error.value Empty)


encode : SignupForm -> Encode.Value
encode form =
    Encode.object
        [ ( "email", Encode.string form.email )
        , ( "firstName", Encode.string form.firstName )
        , ( "lastName", Encode.string form.lastName )
        , ( "password", Encode.string form.password )
        , ( "role", Encode.null )
        ]
