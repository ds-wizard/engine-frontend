module Wizard.Users.Common.UserEditForm exposing
    ( UserEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Common.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Wizard.Api.Models.User exposing (User)


type alias UserEditForm =
    { email : String
    , firstName : String
    , lastName : String
    , affiliation : Maybe String
    , role : String
    , active : Bool
    }


initEmpty : Form FormError UserEditForm
initEmpty =
    Form.initial [] validation


init : User -> Form FormError UserEditForm
init user =
    Form.initial (initUser user) validation


initUser : User -> List ( String, Field.Field )
initUser user =
    [ ( "email", Field.string user.email )
    , ( "firstName", Field.string user.firstName )
    , ( "lastName", Field.string user.lastName )
    , ( "affiliation", Field.maybeString user.affiliation )
    , ( "role", Field.string user.role )
    , ( "active", Field.bool user.active )
    ]


validation : Validation FormError UserEditForm
validation =
    V.succeed UserEditForm
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "firstName" V.string)
        |> V.andMap (V.field "lastName" V.string)
        |> V.andMap (V.field "affiliation" V.maybeString)
        |> V.andMap (V.field "role" V.string)
        |> V.andMap (V.field "active" V.bool)


encode : UuidOrCurrent -> UserEditForm -> E.Value
encode uuidOrCurrent form =
    E.object
        [ ( "uuid", UuidOrCurrent.encode uuidOrCurrent )
        , ( "email", E.string form.email )
        , ( "firstName", E.string form.firstName )
        , ( "lastName", E.string form.lastName )
        , ( "affiliation", E.maybe E.string form.affiliation )
        , ( "role", E.string form.role )
        , ( "active", E.bool form.active )
        ]
