module Public.Signup.Models exposing (..)

import Common.Form.Validate exposing (..)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
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
    }


initEmptySignupForm : Form CustomFormError SignupForm
initEmptySignupForm =
    Form.initial [] signupFormValidation


signupFormValidation : Validation CustomFormError SignupForm
signupFormValidation =
    Validate.map5 SignupForm
        (Validate.field "email" Validate.email)
        (Validate.field "name" Validate.string)
        (Validate.field "surname" Validate.string)
        (Validate.field "password" Validate.string)
        (Validate.field "password" Validate.string |> validateConfirmation "passwordConfirmation")


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
