module Shared.Data.BootstrapConfig exposing
    ( BootstrapConfig
    , authentication
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.BootstrapConfig.AuthenticationConfig as AuthenticationConfig exposing (AuthenticationConfig)


type BootstrapConfig
    = BootstrapConfig Internals


type alias Internals =
    { authentication : AuthenticationConfig }


default : BootstrapConfig
default =
    BootstrapConfig
        { authentication = AuthenticationConfig.default }


authentication : BootstrapConfig -> AuthenticationConfig
authentication (BootstrapConfig config) =
    config.authentication


decoder : Decoder BootstrapConfig
decoder =
    D.succeed Internals
        |> D.required "authentication" AuthenticationConfig.decoder
        |> D.map BootstrapConfig
