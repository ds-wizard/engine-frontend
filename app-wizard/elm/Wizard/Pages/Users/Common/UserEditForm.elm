module Wizard.Pages.Users.Common.UserEditForm exposing
    ( UserEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Common.Utils.Form.Field as Field
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.User exposing (User)


type alias UserEditForm =
    { email : String
    , firstName : String
    , lastName : String
    , affiliation : Maybe String
    , roleUuid : Uuid
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
    , ( "role", Field.string (Uuid.toString user.role.uuid) )
    , ( "active", Field.bool user.active )
    ]


validation : Validation FormError UserEditForm
validation =
    V.succeed UserEditForm
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "firstName" V.string)
        |> V.andMap (V.field "lastName" V.string)
        |> V.andMap (V.field "affiliation" V.maybeString)
        |> V.andMap (V.field "role" V.uuid)
        |> V.andMap (V.field "active" V.bool)


encode : UuidOrCurrent -> UserEditForm -> E.Value
encode uuidOrCurrent form =
    E.object
        [ ( "uuid", UuidOrCurrent.encode uuidOrCurrent )
        , ( "email", E.string form.email )
        , ( "firstName", E.string form.firstName )
        , ( "lastName", E.string form.lastName )
        , ( "affiliation", E.maybe E.string form.affiliation )
        , ( "roleUuid", Uuid.encode form.roleUuid )
        , ( "active", E.bool form.active )
        ]
