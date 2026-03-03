module Wizard.Api.Models.DocumentTemplateSuggestion exposing
    ( DocumentTemplateSuggestion
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormatSimple as DocumentTemplateFormatSimple exposing (DocumentTemplateFormatSimple)


type alias DocumentTemplateSuggestion =
    { uuid : Uuid
    , name : String
    , description : String
    , version : Version
    , formats : List DocumentTemplateFormatSimple
    }


decoder : Decoder DocumentTemplateSuggestion
decoder =
    D.succeed DocumentTemplateSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "version" Version.decoder
        |> D.required "formats" (D.list DocumentTemplateFormatSimple.decoder)
