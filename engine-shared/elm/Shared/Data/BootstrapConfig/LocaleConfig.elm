module Shared.Data.BootstrapConfig.LocaleConfig exposing (LocaleConfig, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias LocaleConfig =
    { code : String
    , name : String
    , defaultLocale : Bool
    }


decoder : Decoder LocaleConfig
decoder =
    D.succeed LocaleConfig
        |> D.required "code" D.string
        |> D.required "name" D.string
        |> D.required "defaultLocale" D.bool
