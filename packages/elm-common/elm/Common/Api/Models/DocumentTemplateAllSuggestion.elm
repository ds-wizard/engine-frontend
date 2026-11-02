module Common.Api.Models.DocumentTemplateAllSuggestion exposing
    ( DocumentTemplateAllSuggestion
    , compare
    , decoder
    )

import Common.Api.Models.DocumentTemplate.DocumentTemplateFormatSimple as DocumentTemplateFormatSimple exposing (DocumentTemplateFormatSimple)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias DocumentTemplateAllSuggestion =
    { uuid : Uuid
    , name : String
    , description : String
    , version : Version
    , formats : List DocumentTemplateFormatSimple
    , id : String
    }


decoder : Decoder DocumentTemplateAllSuggestion
decoder =
    D.succeed DocumentTemplateAllSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "version" Version.decoder
        |> D.required "formats" (D.list DocumentTemplateFormatSimple.decoder)
        |> D.required "id" D.string


compare : DocumentTemplateAllSuggestion -> DocumentTemplateAllSuggestion -> Order
compare a b =
    if a.name == b.name then
        Version.compare a.version b.version

    else
        Basics.compare a.name b.name
