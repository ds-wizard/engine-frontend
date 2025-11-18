module Wizard.Api.Models.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig exposing
    ( EditablePublicKnowledgeModelsConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig.AllowedKnowledgeModelPackage as AllowedKnowledgeModelPackage exposing (AllowedKnowledgeModelPackage)


type alias EditablePublicKnowledgeModelsConfig =
    { enabled : Bool
    , knowledgeModelPackages : List AllowedKnowledgeModelPackage
    }


decoder : Decoder EditablePublicKnowledgeModelsConfig
decoder =
    D.succeed EditablePublicKnowledgeModelsConfig
        |> D.required "enabled" D.bool
        |> D.required "knowledgeModelPackages" (D.list AllowedKnowledgeModelPackage.decoder)


encode : EditablePublicKnowledgeModelsConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "knowledgeModelPackages", E.list AllowedKnowledgeModelPackage.encode config.knowledgeModelPackages )
        ]
