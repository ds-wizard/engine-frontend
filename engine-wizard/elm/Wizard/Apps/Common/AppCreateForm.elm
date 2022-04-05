module Wizard.Apps.Common.AppCreateForm exposing
    ( AppCreateForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)


type alias AppCreateForm =
    { appId : String
    , appName : String
    , email : String
    , firstName : String
    , lastName : String
    }


init : Form FormError AppCreateForm
init =
    Form.initial [] validation


validation : Validation FormError AppCreateForm
validation =
    V.succeed AppCreateForm
        |> V.andMap (V.field "appId" V.string)
        |> V.andMap (V.field "appName" V.string)
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "firstName" V.string)
        |> V.andMap (V.field "lastName" V.string)


encode : AppCreateForm -> E.Value
encode form =
    E.object
        [ ( "appId", E.string form.appId )
        , ( "appName", E.string form.appName )
        , ( "email", E.string form.email )
        , ( "firstName", E.string form.firstName )
        , ( "lastName", E.string form.lastName )
        , ( "password", E.string "" )
        ]
