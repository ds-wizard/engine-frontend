module Wizard.Common.Config.AuthConfig exposing
    ( AuthConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Config.AuthServiceConfig as AuthServiceConfig exposing (AuthServiceConfig)
import Wizard.Common.Config.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)


type alias AuthConfig =
    { external : External
    , internal : Internal
    }


type alias External =
    { services : List AuthServiceConfig }


type alias Internal =
    { registration : SimpleFeatureConfig }


decoder : Decoder AuthConfig
decoder =
    D.succeed AuthConfig
        |> D.required "external" externalDecoder
        |> D.required "internal" internalDecoder


externalDecoder : Decoder External
externalDecoder =
    D.succeed External
        |> D.required "services" (D.list AuthServiceConfig.decoder)


internalDecoder : Decoder Internal
internalDecoder =
    D.succeed Internal
        |> D.required "registration" SimpleFeatureConfig.decoder


default : AuthConfig
default =
    { external = { services = [] }
    , internal = { registration = SimpleFeatureConfig.enabled }
    }
