module Wizard.Pages.Users.Common.ApiKeyCreateForm exposing
    ( ApiKeyCreateForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)


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
