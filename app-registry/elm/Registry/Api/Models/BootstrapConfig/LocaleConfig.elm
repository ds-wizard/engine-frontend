module Registry.Api.Models.BootstrapConfig.LocaleConfig exposing (LocaleConfig, decoder, default)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias LocaleConfig =
    { enabled : Bool }


default : LocaleConfig
default =
    { enabled = True }


decoder : Decoder LocaleConfig
decoder =
    D.succeed LocaleConfig
        |> D.required "enabled" D.bool
