module Common.Api.Models.BootstrapConfig.RegistryConfig exposing
    ( RegistryConfig(..)
    , decoder
    , default
    , isEnabled
    , unconfigured
    , url
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type RegistryConfig
    = RegistryEnabled String
    | RegistryDisabled
    | RegistryUnconfigured


decoder : Decoder RegistryConfig
decoder =
    D.succeed Tuple.pair
        |> D.required "enabled" D.bool
        |> D.required "url" (D.nullable D.string)
        |> D.map
            (\( enabled, mbUrl ) ->
                case ( enabled, mbUrl ) of
                    ( True, Just registryUrl ) ->
                        RegistryEnabled registryUrl

                    _ ->
                        RegistryDisabled
            )


default : RegistryConfig
default =
    RegistryDisabled


{-| The registry of a server whose configuration has no registry section: there
is nothing to switch on or off, so it is on, at the address the client knows
its registry by (`url`).
-}
unconfigured : RegistryConfig
unconfigured =
    RegistryUnconfigured


{-| Where the registry is, if it is on: the address the server gives, or, where
it gives none, the client's own (`appState.clientDefaults.registryUrl`).
-}
url : String -> RegistryConfig -> Maybe String
url clientRegistryUrl registryConfig =
    case registryConfig of
        RegistryEnabled registryUrl ->
            Just registryUrl

        RegistryUnconfigured ->
            Just clientRegistryUrl

        RegistryDisabled ->
            Nothing


isEnabled : RegistryConfig -> Bool
isEnabled registryConfig =
    case registryConfig of
        RegistryEnabled _ ->
            True

        RegistryUnconfigured ->
            True

        RegistryDisabled ->
            False
