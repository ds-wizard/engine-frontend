module Wizard.Api.Models.KnowledgeModel.Integration.IntegrationType exposing
    ( IntegrationType(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type IntegrationType
    = Api
    | Plugin


decoder : Decoder IntegrationType
decoder =
    D.field "integrationType" D.string
        |> D.andThen
            (\str ->
                case str of
                    "ApiIntegration" ->
                        D.succeed Api

                    "PluginIntegration" ->
                        D.succeed Plugin

                    valueType ->
                        D.fail <| "Unknown integration type: " ++ valueType
            )
