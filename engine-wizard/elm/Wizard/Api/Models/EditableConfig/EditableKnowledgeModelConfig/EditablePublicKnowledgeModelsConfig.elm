module Wizard.Api.Models.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig exposing
    ( EditablePublicKnowledgeModelsConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.EditableConfig.EditableKnowledgeModelConfig.EditablePublicKnowledgeModelsConfig.AllowedPackage as AllowedPackage exposing (AllowedPackage)


type alias EditablePublicKnowledgeModelsConfig =
    { enabled : Bool
    , packages : List AllowedPackage
    }


decoder : Decoder EditablePublicKnowledgeModelsConfig
decoder =
    D.succeed EditablePublicKnowledgeModelsConfig
        |> D.required "enabled" D.bool
        |> D.required "packages" (D.list AllowedPackage.decoder)


encode : EditablePublicKnowledgeModelsConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "packages", E.list AllowedPackage.encode config.packages )
        ]
