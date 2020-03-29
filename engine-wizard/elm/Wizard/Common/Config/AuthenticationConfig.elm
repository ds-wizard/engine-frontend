module Wizard.Common.Config.AuthenticationConfig exposing
    ( AuthenticationConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Config.Partials.OpenIDServiceConfig as OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Wizard.Common.Config.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Wizard.Users.Common.Role as Role


type alias AuthenticationConfig =
    { defaultRole : String
    , internal : Internal
    , external : External
    }


type alias Internal =
    { registration : SimpleFeatureConfig }


type alias External =
    { services : List OpenIDServiceConfig }


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


default : AuthenticationConfig
default =
    { defaultRole = Role.researcher
    , internal = { registration = SimpleFeatureConfig.enabled }
    , external = { services = [] }
    }
