module Registry.Common.Entities.BootstrapConfig.AuthenticationConfig exposing
    ( AuthenticationConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias AuthenticationConfig =
    { publicRegistrationEnabled : Bool }


default : AuthenticationConfig
default =
    { publicRegistrationEnabled = True }


decoder : Decoder AuthenticationConfig
decoder =
    D.succeed AuthenticationConfig
        |> D.required "publicRegistrationEnabled" D.bool
