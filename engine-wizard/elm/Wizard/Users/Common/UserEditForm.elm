module Wizard.Users.Common.UserEditForm exposing
    ( UserEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (..)
import Json.Encode as E exposing (..)
import Json.Encode.Extra as E
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Field as Field
import Wizard.Common.Form.Validate as V
import Wizard.Users.Common.User exposing (User)


type alias UserEditForm =
    { email : String
    , firstName : String
    , lastName : String
    , affiliation : Maybe String
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
    V.succeed UserEditForm
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "firstName" V.string)
        |> V.andMap (V.field "lastName" V.string)
        |> V.andMap (V.field "affiliation" V.maybeString)
        |> V.andMap (V.field "role" V.string)
        |> V.andMap (V.field "active" V.bool)


encode : String -> UserEditForm -> E.Value
encode uuid form =
    E.object
        [ ( "uuid", E.string uuid )
        , ( "email", E.string form.email )
        , ( "firstName", E.string form.firstName )
        , ( "lastName", E.string form.lastName )
        , ( "affiliation", E.maybe E.string form.affiliation )
        , ( "role", E.string form.role )
        , ( "active", E.bool form.active )
        ]


userToUserEditFormInitials : User -> List ( String, Field.Field )
userToUserEditFormInitials user =
    [ ( "email", Field.string user.email )
    , ( "firstName", Field.string user.firstName )
    , ( "lastName", Field.string user.lastName )
    , ( "affiliation", Field.maybeString user.affiliation )
    , ( "role", Field.string user.role )
    , ( "active", Field.bool user.active )
    ]
