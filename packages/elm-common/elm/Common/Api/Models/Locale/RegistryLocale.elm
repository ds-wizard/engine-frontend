module Common.Api.Models.Locale.RegistryLocale exposing
    ( RegistryLocale
    , decoder
    )

{-| A translation the registry offers for the running version, and whether a
translation of its language is already here.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias RegistryLocale =
    { code : String
    , name : String
    , remoteVersion : String
    , installed : Bool
    }


decoder : Decoder RegistryLocale
decoder =
    D.succeed RegistryLocale
        |> D.required "code" D.string
        |> D.required "name" D.string
        |> D.required "remoteVersion" D.string
        |> D.required "installed" D.bool
