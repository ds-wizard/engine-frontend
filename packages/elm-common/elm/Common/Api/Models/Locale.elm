module Common.Api.Models.Locale exposing
    ( Locale
    , decoder
    , version
    )

{-| A translation, of the user interface or of the mails: one PO file
per language code, everything about it from the PO's header. Built-in English
is not one of them.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import List.Extra as List
import Time
import Uuid exposing (Uuid)


type alias Locale =
    { uuid : Uuid
    , code : String
    , name : String
    , potVersion : Maybe String
    , enabled : Bool
    , defaultLocale : Bool
    , official : Bool
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


decoder : Decoder Locale
decoder =
    D.succeed Locale
        |> D.required "uuid" Uuid.decoder
        |> D.required "code" D.string
        |> D.required "name" D.string
        |> D.optional "potVersion" (D.nullable D.string) Nothing
        |> D.required "enabled" D.bool
        |> D.required "defaultLocale" D.bool
        |> D.required "official" D.bool
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime


{-| The version of a `<kind>:<version>` POT version: what follows the last `:`.
-}
version : String -> String
version potVersion =
    potVersion
        |> String.split ":"
        |> List.last
        |> Maybe.withDefault potVersion
