module Wizard.Api.Models.EditableConfig.EditableKnowledgeModelConfig exposing
    ( EditableKnowledgeModelConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias EditableKnowledgeModelConfig =
    { integrationConfig : String
    }


decoder : Decoder EditableKnowledgeModelConfig
decoder =
    D.succeed EditableKnowledgeModelConfig
        |> D.required "integrationConfig" D.string


encode : EditableKnowledgeModelConfig -> E.Value
encode config =
    E.object
        [ ( "integrationConfig", E.string config.integrationConfig )
        ]
