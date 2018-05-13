module Users.Edit.Models exposing (..)

import Common.Form exposing (CustomFormError)
import Common.Form.Validate exposing (validateConfirmation)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)
import Users.Common.Models exposing (User)


type View
    = Profile
    | Password


type alias Model =
    { uuid : String
    , currentView : View
    , user : ActionResult User
    , savingUser : ActionResult String
    , savingPassword : ActionResult String
    , userForm : Form CustomFormError UserEditForm
    , passwordForm : Form CustomFormError UserPasswordForm
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , currentView = Profile
    , user = Loading
    , savingUser = Unset
    , savingPassword = Unset
    , userForm = initEmptyUserEditForm
    , passwordForm = initUserPasswordForm
    }


type alias UserEditForm =
    { email : String
    , name : String
    , surname : String
    , role : String
    , isActive : Bool
    }


initEmptyUserEditForm : Form CustomFormError UserEditForm
initEmptyUserEditForm =
    Form.initial [] userEditFormValidation


initUserEditForm : User -> Form CustomFormError UserEditForm
initUserEditForm user =
    Form.initial (userToUserEditFormInitials user) userEditFormValidation


userEditFormValidation : Validation CustomFormError UserEditForm
userEditFormValidation =
    Validate.map5 UserEditForm
        (Validate.field "email" Validate.email)
        (Validate.field "name" Validate.string)
        (Validate.field "surname" Validate.string)
        (Validate.field "role" Validate.string)
        (Validate.field "isActive" Validate.bool)


encodeUserEditForm : String -> UserEditForm -> Encode.Value
encodeUserEditForm uuid form =
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "email", Encode.string form.email )
        , ( "name", Encode.string form.name )
        , ( "surname", Encode.string form.surname )
        , ( "role", Encode.string form.role )
        , ( "isActive", Encode.bool form.isActive )
        ]


userToUserEditFormInitials : User -> List ( String, Field.Field )
userToUserEditFormInitials user =
    [ ( "email", Field.string user.email )
    , ( "name", Field.string user.name )
    , ( "surname", Field.string user.surname )
    , ( "role", Field.string user.role )
    , ( "isActive", Field.bool user.isActive )
    ]


type alias UserPasswordForm =
    { password : String
    , passwordConfirmation : String
    }


initUserPasswordForm : Form CustomFormError UserPasswordForm
initUserPasswordForm =
    Form.initial [] userPasswordFormValidation


userPasswordFormValidation : Validation CustomFormError UserPasswordForm
userPasswordFormValidation =
    Validate.map2 UserPasswordForm
        (Validate.field "password" Validate.string)
        (Validate.field "password" Validate.string |> validateConfirmation "passwordConfirmation")


encodeUserPasswordForm : UserPasswordForm -> Encode.Value
encodeUserPasswordForm form =
    Encode.object
        [ ( "password", Encode.string form.password )
        ]
