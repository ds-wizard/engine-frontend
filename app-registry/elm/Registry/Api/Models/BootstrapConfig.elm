module Registry.Api.Models.BootstrapConfig exposing
    ( BootstrapConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Registry.Api.Models.BootstrapConfig.AuthenticationConfig as AuthenticationConfig exposing (AuthenticationConfig)
import Registry.Api.Models.BootstrapConfig.LocaleConfig as LocaleConfig exposing (LocaleConfig)


type alias BootstrapConfig =
    { authentication : AuthenticationConfig
    , locale : LocaleConfig
    }


default : BootstrapConfig
default =
    { authentication = AuthenticationConfig.default
    , locale = LocaleConfig.default
    }


decoder : Decoder BootstrapConfig
decoder =
    D.succeed BootstrapConfig
        |> D.required "authentication" AuthenticationConfig.decoder
        |> D.required "locale" LocaleConfig.decoder
