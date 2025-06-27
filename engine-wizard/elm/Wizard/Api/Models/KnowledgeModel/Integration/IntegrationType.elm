module Wizard.Api.Models.KnowledgeModel.Integration.IntegrationType exposing
    ( IntegrationType(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type IntegrationType
    = Api
    | Widget


decoder : Decoder IntegrationType
decoder =
    D.field "integrationType" D.string
        |> D.andThen
            (\str ->
                case str of
                    "ApiIntegration" ->
                        D.succeed Api

                    "WidgetIntegration" ->
                        D.succeed Widget

                    valueType ->
                        D.fail <| "Unknown integration type: " ++ valueType
            )
