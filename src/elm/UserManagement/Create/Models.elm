module UserManagement.Create.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)


type alias Model =
    { savingUser : ActionResult String
    , form : Form () UserCreateForm
    }


initialModel : Model
initialModel =
    { savingUser = Unset
    , form = initUserCreateForm
    }


type alias UserCreateForm =
    { email : String
    , name : String
    , surname : String
    , role : String
    , password : String
    }


initUserCreateForm : Form () UserCreateForm
initUserCreateForm =
    Form.initial [] userCreateFormValidation


userCreateFormValidation : Validation () UserCreateForm
userCreateFormValidation =
    Validate.map5 UserCreateForm
        (Validate.field "email" Validate.email)
        (Validate.field "name" Validate.string)
        (Validate.field "surname" Validate.string)
        (Validate.field "role" Validate.string)
        (Validate.field "password" Validate.string)


encodeUserCreateForm : String -> UserCreateForm -> Encode.Value
encodeUserCreateForm uuid form =
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "email", Encode.string form.email )
        , ( "name", Encode.string form.name )
        , ( "surname", Encode.string form.surname )
        , ( "role", Encode.string form.role )
        , ( "password", Encode.string form.password )
        ]
