module Wizard.Pages.KnowledgeModelSecrets.Forms.KnowledgeModelSecretForm exposing
    ( KnowledgeModelSecretForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Form.Validate as V
import Wizard.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Wizard.Data.AppState exposing (AppState)


type alias KnowledgeModelSecretForm =
    { name : String
    , value : String
    }


init : AppState -> KnowledgeModelSecret -> Form FormError KnowledgeModelSecretForm
init appState secret =
    let
        initialData =
            [ ( "name", Field.string secret.name )
            , ( "value", Field.string secret.value )
            ]
    in
    Form.initial initialData (validation appState)


initEmpty : AppState -> Form FormError KnowledgeModelSecretForm
initEmpty appState =
    Form.initial [] (validation appState)


validation : AppState -> Validation FormError KnowledgeModelSecretForm
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
