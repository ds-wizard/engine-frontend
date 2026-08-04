module Wizard.Pages.Settings.Common.Forms.RoleForm exposing
    ( RoleForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Common.Api.Models.Role exposing (Role)
import Common.Api.Models.RolePermission as RolePermission exposing (RolePermission)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E


type alias RoleForm =
    { name : String
    }


initEmpty : Form FormError RoleForm
initEmpty =
    Form.initial [] validation


init : Role -> Form FormError RoleForm
init role =
    Form.initial
        [ ( "name", Field.string role.name )
        ]
        validation


validation : Validation FormError RoleForm
validation =
    V.succeed RoleForm
        |> V.andMap (V.field "name" V.string)


encode : List RolePermission -> RoleForm -> E.Value
encode permissions form =
    E.object
        [ ( "name", E.string form.name )
        , ( "permissions", E.list RolePermission.encode permissions )
        ]
