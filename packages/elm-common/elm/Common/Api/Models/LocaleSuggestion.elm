module Common.Api.Models.LocaleSuggestion exposing
    ( LocaleSuggestion
    , decoder
    )

{-| An enabled translation of the user interface, as the language picker offers
it.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias LocaleSuggestion =
    { uuid : Uuid
    , code : String
    , name : String
    , defaultLocale : Bool
    }


decoder : Decoder LocaleSuggestion
decoder =
    D.succeed LocaleSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "code" D.string
        |> D.required "name" D.string
        |> D.required "defaultLocale" D.bool
