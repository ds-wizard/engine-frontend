module Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing
    ( RegistryConfig(..)
    , decoder
    , default
    , isEnabled
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
        |> D.map
            (\( enabled, mbUrl ) ->
                case ( enabled, mbUrl ) of
                    ( True, Just url ) ->
                        RegistryEnabled url

                    _ ->
                        RegistryDisabled
            )


default : RegistryConfig
default =
    RegistryDisabled


isEnabled : RegistryConfig -> Bool
isEnabled registryConfig =
    case registryConfig of
        RegistryEnabled _ ->
            True

        _ ->
            False
