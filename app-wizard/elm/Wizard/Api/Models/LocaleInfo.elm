module Wizard.Api.Models.LocaleInfo exposing
    ( LocaleInfo
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias LocaleInfo =
    { uuid : Uuid
    , name : String
    , code : String
    , defaultLocale : Bool
    }


decoder : Decoder LocaleInfo
decoder =
    D.succeed LocaleInfo
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "code" D.string
        |> D.required "defaultLocale" D.bool
