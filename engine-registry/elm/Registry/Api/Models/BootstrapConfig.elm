module Registry.Api.Models.BootstrapConfig exposing
    ( BootstrapConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Registry.Api.Models.BootstrapConfig.AuthenticationConfig as AuthenticationConfig exposing (AuthenticationConfig)


type alias BootstrapConfig =
    { authentication : AuthenticationConfig
    }


default : BootstrapConfig
default =
    { authentication = AuthenticationConfig.default }


decoder : Decoder BootstrapConfig
decoder =
    D.succeed BootstrapConfig
        |> D.required "authentication" AuthenticationConfig.decoder
