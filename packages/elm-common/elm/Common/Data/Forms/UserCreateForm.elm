module Common.Data.Forms.UserCreateForm exposing
    ( UserCreateForm
    , encode
    , init
    , validation
    )

import Common.Data.AppState exposing (AppState)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)


type alias UserCreateForm =
    { email : String
    , firstName : String
    , lastName : String
    , affiliation : Maybe String
    , roleUuid : Uuid
    , password : String
    }


init : AppState -> Form FormError UserCreateForm
init appState =
    Form.initial [] (validation appState)


validation : AppState -> Validation FormError UserCreateForm
validation appState =
    V.succeed UserCreateForm
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "firstName" V.string)
        |> V.andMap (V.field "lastName" V.string)
        |> V.andMap (V.field "affiliation" V.maybeString)
        |> V.andMap (V.field "roleUuid" V.uuid)
        |> V.andMap (V.field "password" (V.password appState))


encode : String -> UserCreateForm -> E.Value
encode uuid form =
    E.object
        [ ( "uuid", E.string uuid )
        , ( "email", E.string form.email )
        , ( "firstName", E.string form.firstName )
        , ( "lastName", E.string form.lastName )
        , ( "affiliation", E.maybe E.string form.affiliation )
        , ( "roleUuid", Uuid.encode form.roleUuid )
        , ( "password", E.string form.password )
        ]
