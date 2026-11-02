module Common.Data.Forms.KnowledgeModelSecretForm exposing
    ( KnowledgeModelSecretForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Common.Api.Context exposing (Context)
import Common.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E


type alias KnowledgeModelSecretForm =
    { name : String
    , value : String
    }


init : Context ctx -> KnowledgeModelSecret -> Form FormError KnowledgeModelSecretForm
init appState secret =
    Form.initial [ ( "name", Field.string secret.name ) ] (validation appState)


initEmpty : Context ctx -> Form FormError KnowledgeModelSecretForm
initEmpty appState =
    Form.initial [] (validation appState)


validation : Context ctx -> Validation FormError KnowledgeModelSecretForm
validation appState =
    V.succeed KnowledgeModelSecretForm
        |> V.andMap (V.field "name" (V.kmSecret appState))
        |> V.andMap (V.field "value" V.string)


encode : KnowledgeModelSecretForm -> E.Value
encode form =
    E.object
        [ ( "name", E.string form.name )
        , ( "value", E.string form.value )
        ]
