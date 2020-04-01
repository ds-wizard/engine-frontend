module Wizard.Settings.Common.EditableKnowledgeModelRegistryConfig exposing
    ( EditableKnowledgeModelRegistryConfig
    , decoder
    , encode
    , initEmptyForm
    , initForm
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate as V


type alias EditableKnowledgeModelRegistryConfig =
    { enabled : Bool
    , token : String
    }



-- JSON


decoder : Decoder EditableKnowledgeModelRegistryConfig
decoder =
    D.succeed EditableKnowledgeModelRegistryConfig
        |> D.required "enabled" D.bool
        |> D.required "token" D.string


encode : EditableKnowledgeModelRegistryConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "token", E.string config.token )
        ]



-- Form


validation : Validation CustomFormError EditableKnowledgeModelRegistryConfig
validation =
    V.succeed EditableKnowledgeModelRegistryConfig
        |> V.andMap (V.field "enabled" V.bool)
        |> V.andMap (V.field "enabled" V.bool |> V.ifElse "token" V.string V.optionalString)


initEmptyForm : Form CustomFormError EditableKnowledgeModelRegistryConfig
initEmptyForm =
    Form.initial [] validation


initForm : EditableKnowledgeModelRegistryConfig -> Form CustomFormError EditableKnowledgeModelRegistryConfig
initForm config =
    let
        fields =
            [ ( "enabled", Field.bool config.enabled )
            , ( "token", Field.string config.token )
            ]
    in
    Form.initial fields validation
