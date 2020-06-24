module Shared.Data.BootstrapConfig.AuthenticationConfig exposing
    ( AuthenticationConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Auth.Role as Role
import Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig as OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Shared.Data.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)


type alias AuthenticationConfig =
    { defaultRole : String
    , internal : Internal
    , external : External
    }


type alias Internal =
    { registration : SimpleFeatureConfig }


type alias External =
    { services : List OpenIDServiceConfig }


default : AuthenticationConfig
default =
    { defaultRole = Role.researcher
    , internal = { registration = SimpleFeatureConfig.init True }
    , external = { services = [] }
    }


decoder : Decoder AuthenticationConfig
decoder =
    D.succeed AuthenticationConfig
        |> D.required "defaultRole" D.string
        |> D.required "internal" internalDecoder
        |> D.required "external" externalDecoder


internalDecoder : Decoder Internal
internalDecoder =
    D.succeed Internal
        |> D.required "registration" SimpleFeatureConfig.decoder


externalDecoder : Decoder External
externalDecoder =
    D.succeed External
        |> D.required "services" (D.list OpenIDServiceConfig.decoder)
