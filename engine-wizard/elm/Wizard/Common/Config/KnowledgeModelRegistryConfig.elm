module Wizard.Common.Config.KnowledgeModelRegistryConfig exposing
    ( KnowledgeModelRegistryConfig(..)
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type KnowledgeModelRegistryConfig
    = KnowledgeModelRegistryEnabled String
    | KnowledgeModelRegistryDisabled


decoder : Decoder KnowledgeModelRegistryConfig
decoder =
    D.succeed Tuple.pair
        |> D.required "enabled" D.bool
        |> D.required "url" (D.maybe D.string)
        |> D.andThen
            (\( enabled, mbUrl ) ->
                case ( enabled, mbUrl ) of
                    ( True, Just url ) ->
                        D.succeed <| KnowledgeModelRegistryEnabled url

                    _ ->
                        D.succeed KnowledgeModelRegistryDisabled
            )


default : KnowledgeModelRegistryConfig
default =
    KnowledgeModelRegistryDisabled
