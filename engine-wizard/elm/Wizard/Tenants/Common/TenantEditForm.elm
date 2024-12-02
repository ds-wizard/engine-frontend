module Wizard.Tenants.Common.TenantEditForm exposing (TenantEditForm, encode, init, validation)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Data.TenantDetail exposing (TenantDetail)
import Shared.Form.FormError exposing (FormError)


type alias TenantEditForm =
    { tenantId : String
    , name : String
    }


init : TenantDetail -> Form FormError TenantEditForm
init tenantDetail =
    let
        fields =
            [ ( "tenantId", Field.string tenantDetail.tenantId )
            , ( "name", Field.string tenantDetail.name )
            ]
    in
    Form.initial fields validation


validation : Validation FormError TenantEditForm
validation =
    V.succeed TenantEditForm
        |> V.andMap (V.field "tenantId" V.string)
        |> V.andMap (V.field "name" V.string)


encode : TenantEditForm -> E.Value
encode form =
    E.object
        [ ( "tenantId", E.string form.tenantId )
        , ( "name", E.string form.name )
        ]
