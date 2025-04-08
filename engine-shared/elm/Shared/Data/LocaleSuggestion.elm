module Shared.Data.LocaleSuggestion exposing (LocaleSuggestion, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias LocaleSuggestion =
    { code : String
    , defaultLocale : Bool
    , description : String
    , id : String
    , name : String
    }


decoder : Decoder LocaleSuggestion
decoder =
    D.succeed LocaleSuggestion
        |> D.required "code" D.string
        |> D.required "defaultLocale" D.bool
        |> D.required "description" D.string
        |> D.required "id" D.string
        |> D.required "name" D.string
