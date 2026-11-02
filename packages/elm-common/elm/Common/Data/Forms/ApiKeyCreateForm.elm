module Common.Data.Forms.ApiKeyCreateForm exposing
    ( ApiKeyCreateForm
    , encode
    , init
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E


type alias ApiKeyCreateForm =
    { name : String
    , expiresAt : String
    }


init : Form FormError ApiKeyCreateForm
init =
    Form.initial [] validation


validation : Validation FormError ApiKeyCreateForm
validation =
    V.succeed ApiKeyCreateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "expiresAt" V.string)


encode : ApiKeyCreateForm -> E.Value
encode form =
    E.object
        [ ( "name", E.string form.name )
        , ( "expiresAt", E.string form.expiresAt )
        ]
