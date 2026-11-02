module Common.Data.Forms.TenantEditForm exposing (TenantEditForm, encode, encodeWith, init, validation)

import Common.Api.Models.TenantDetail exposing (TenantDetail)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E


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
encode =
    encodeWith []


{-| With the fields a server takes beside the form's, as they are now.
-}
encodeWith : List ( String, E.Value ) -> TenantEditForm -> E.Value
encodeWith fields form =
    E.object
        (( "tenantId", E.string form.tenantId )
            :: ( "name", E.string form.name )
            :: fields
        )
