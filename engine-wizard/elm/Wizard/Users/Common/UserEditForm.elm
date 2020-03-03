module Wizard.Users.Common.UserEditForm exposing
    ( UserEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Users.Common.User exposing (User)


type alias UserEditForm =
    { email : String
    , firstName : String
    , lastName : String
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
        (Validate.field "firstName" Validate.string)
        (Validate.field "lastName" Validate.string)
        (Validate.field "role" Validate.string)
        (Validate.field "active" Validate.bool)


encode : String -> UserEditForm -> Encode.Value
encode uuid form =
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "email", Encode.string form.email )
        , ( "firstName", Encode.string form.firstName )
        , ( "lastName", Encode.string form.lastName )
        , ( "role", Encode.string form.role )
        , ( "active", Encode.bool form.active )
        ]


userToUserEditFormInitials : User -> List ( String, Field.Field )
userToUserEditFormInitials user =
    [ ( "email", Field.string user.email )
    , ( "firstName", Field.string user.firstName )
    , ( "lastName", Field.string user.lastName )
    , ( "role", Field.string user.role )
    , ( "active", Field.bool user.active )
    ]
