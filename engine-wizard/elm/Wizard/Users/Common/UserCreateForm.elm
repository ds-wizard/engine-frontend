module Wizard.Users.Common.UserCreateForm exposing (UserCreateForm, encode, init, validation)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (..)
import Json.Encode as E exposing (..)
import Json.Encode.Extra as E
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate as V


type alias UserCreateForm =
    { email : String
    , firstName : String
    , lastName : String
    , affiliation : Maybe String
    , role : String
    , password : String
    }


init : AppState -> Form CustomFormError UserCreateForm
init appState =
    let
        fields =
            [ ( "role", Field.string appState.config.authentication.defaultRole ) ]
    in
    Form.initial fields validation


validation : Validation CustomFormError UserCreateForm
validation =
    V.succeed UserCreateForm
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "firstName" V.string)
        |> V.andMap (V.field "lastName" V.string)
        |> V.andMap (V.field "affiliation" V.maybeString)
        |> V.andMap (V.field "role" V.string)
        |> V.andMap (V.field "password" V.string)


encode : String -> UserCreateForm -> E.Value
encode uuid form =
    E.object
        [ ( "uuid", E.string uuid )
        , ( "email", E.string form.email )
        , ( "firstName", E.string form.firstName )
        , ( "lastName", E.string form.lastName )
        , ( "affiliation", E.maybe E.string form.affiliation )
        , ( "role", E.string form.role )
        , ( "password", E.string form.password )
        ]
