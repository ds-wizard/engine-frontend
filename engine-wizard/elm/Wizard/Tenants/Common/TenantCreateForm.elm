module Wizard.Tenants.Common.TenantCreateForm exposing
    ( TenantCreateForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)


type alias TenantCreateForm =
    { tenantId : String
    , tenantName : String
    , email : String
    , firstName : String
    , lastName : String
    }


init : Form FormError TenantCreateForm
init =
    Form.initial [] validation


validation : Validation FormError TenantCreateForm
validation =
    V.succeed TenantCreateForm
        |> V.andMap (V.field "tenantId" V.string)
        |> V.andMap (V.field "tenantName" V.string)
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "firstName" V.string)
        |> V.andMap (V.field "lastName" V.string)


encode : TenantCreateForm -> E.Value
encode form =
    E.object
        [ ( "tenantId", E.string form.tenantId )
        , ( "tenantName", E.string form.tenantName )
        , ( "email", E.string form.email )
        , ( "firstName", E.string form.firstName )
        , ( "lastName", E.string form.lastName )
        , ( "password", E.string "" )
        ]
