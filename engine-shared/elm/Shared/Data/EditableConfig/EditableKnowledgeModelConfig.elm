module Shared.Data.EditableConfig.EditableKnowledgeModelConfig exposing
    ( EditableKnowledgeModelConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig as EditablePublicKnowledgeModelsConfig exposing (EditablePublicKnowledgeModelsConfig)


type alias EditableKnowledgeModelConfig =
    { integrationConfig : String
    , public : EditablePublicKnowledgeModelsConfig
    }


decoder : Decoder EditableKnowledgeModelConfig
decoder =
    D.succeed EditableKnowledgeModelConfig
        |> D.required "integrationConfig" D.string
        |> D.required "public" EditablePublicKnowledgeModelsConfig.decoder


encode : EditableKnowledgeModelConfig -> E.Value
encode config =
    E.object
        [ ( "integrationConfig", E.string config.integrationConfig )
        , ( "public", EditablePublicKnowledgeModelsConfig.encode config.public )
        ]
