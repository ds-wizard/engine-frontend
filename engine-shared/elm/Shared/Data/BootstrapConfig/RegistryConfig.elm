module Shared.Data.BootstrapConfig.RegistryConfig exposing
    ( RegistryConfig(..)
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type RegistryConfig
    = RegistryEnabled String
    | RegistryDisabled


decoder : Decoder RegistryConfig
decoder =
    D.succeed Tuple.pair
        |> D.required "enabled" D.bool
        |> D.required "url" (D.maybe D.string)
        |> D.andThen
            (\( enabled, mbUrl ) ->
                case ( enabled, mbUrl ) of
                    ( True, Just url ) ->
                        D.succeed <| RegistryEnabled url

                    _ ->
                        D.succeed RegistryDisabled
            )


default : RegistryConfig
default =
    RegistryDisabled
