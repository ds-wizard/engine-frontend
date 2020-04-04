module Shared.Data.BootstrapConfig.AuthenticationConfig exposing (AuthenticationConfig, decoder, default, services)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig as OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Shared.Data.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)


type AuthenticationConfig
    = AuthenticationConfig Internals


type alias Internals =
    { internal : Internal
    , external : External
    }


type alias Internal =
    { registration : SimpleFeatureConfig }


type alias External =
    { services : List OpenIDServiceConfig }


default : AuthenticationConfig
default =
    AuthenticationConfig
        { internal = { registration = SimpleFeatureConfig.init True }
        , external = { services = [] }
        }


services : AuthenticationConfig -> List OpenIDServiceConfig
services (AuthenticationConfig config) =
    config.external.services


decoder : Decoder AuthenticationConfig
decoder =
    D.succeed Internals
        |> D.required "internal" internalDecoder
        |> D.required "external" externalDecoder
        |> D.map AuthenticationConfig


internalDecoder : Decoder Internal
internalDecoder =
    D.succeed Internal
        |> D.required "registration" SimpleFeatureConfig.decoder


externalDecoder : Decoder External
externalDecoder =
    D.succeed External
        |> D.required "services" (D.list OpenIDServiceConfig.decoder)
