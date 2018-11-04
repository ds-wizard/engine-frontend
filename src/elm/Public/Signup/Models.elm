module Public.Signup.Models exposing (Model, SignupForm, encodeSignupForm, initEmptySignupForm, initialModel, signupFormValidation, validateAcceptField)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Common.Form.Validate exposing (..)
import Form exposing (Form)
import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)


type alias Model =
    { form : Form CustomFormError SignupForm
    , signingUp : ActionResult String
    }


initialModel : Model
initialModel =
    { form = initEmptySignupForm
    , signingUp = Unset
    }


type alias SignupForm =
    { email : String
    , name : String
    , surname : String
    , password : String
    , passwordConfirmation : String
    , accept : Bool
    }


initEmptySignupForm : Form CustomFormError SignupForm
initEmptySignupForm =
    Form.initial [] signupFormValidation


signupFormValidation : Validation CustomFormError SignupForm
signupFormValidation =
    Validate.map6 SignupForm
        (Validate.field "email" Validate.email)
        (Validate.field "name" Validate.string)
        (Validate.field "surname" Validate.string)
        (Validate.field "password" Validate.string)
        (Validate.field "password" Validate.string |> validateConfirmation "passwordConfirmation")
        (Validate.field "accept" validateAcceptField)


validateAcceptField : Field -> Result (Error customError) Bool
validateAcceptField v =
    if Field.asBool v |> Maybe.withDefault False then
        Ok True
    else
        Err (Error.value Empty)


encodeSignupForm : String -> SignupForm -> Encode.Value
encodeSignupForm uuid form =
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "email", Encode.string form.email )
        , ( "name", Encode.string form.name )
        , ( "surname", Encode.string form.surname )
        , ( "password", Encode.string form.password )
        , ( "role", Encode.null )
        ]
