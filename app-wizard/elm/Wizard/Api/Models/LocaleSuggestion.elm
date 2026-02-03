module Wizard.Api.Models.LocaleSuggestion exposing (LocaleSuggestion, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias LocaleSuggestion =
    { code : String
    , defaultLocale : Bool
    , description : String
    , uuid : Uuid
    , name : String
    }


decoder : Decoder LocaleSuggestion
decoder =
    D.succeed LocaleSuggestion
        |> D.required "code" D.string
        |> D.required "defaultLocale" D.bool
        |> D.required "description" D.string
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
