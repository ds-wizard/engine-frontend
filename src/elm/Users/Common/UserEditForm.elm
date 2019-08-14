module Users.Common.UserEditForm exposing
    ( UserEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)
import Users.Common.User exposing (User)


type alias UserEditForm =
    { email : String
    , name : String
    , surname : String
    , role : String
    , active : Bool
    }


initEmpty : Form CustomFormError UserEditForm
initEmpty =
    Form.initial [] validation


init : User -> Form CustomFormError UserEditForm
init user =
    Form.initial (userToUserEditFormInitials user) validation


validation : Validation CustomFormError UserEditForm
validation =
    Validate.map5 UserEditForm
        (Validate.field "email" Validate.email)
        (Validate.field "name" Validate.string)
        (Validate.field "surname" Validate.string)
        (Validate.field "role" Validate.string)
        (Validate.field "active" Validate.bool)


encode : String -> UserEditForm -> Encode.Value
encode uuid form =
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "email", Encode.string form.email )
        , ( "name", Encode.string form.name )
        , ( "surname", Encode.string form.surname )
        , ( "role", Encode.string form.role )
        , ( "active", Encode.bool form.active )
        ]


userToUserEditFormInitials : User -> List ( String, Field.Field )
userToUserEditFormInitials user =
    [ ( "email", Field.string user.email )
    , ( "name", Field.string user.name )
    , ( "surname", Field.string user.surname )
    , ( "role", Field.string user.role )
    , ( "active", Field.bool user.active )
    ]
