module Wizard.Api.Models.DocumentTemplateAllSuggestion exposing
    ( DocumentTemplateAllSuggestion
    , compare
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormatSimple as DocumentTemplateFormatSimple exposing (DocumentTemplateFormatSimple)


type alias DocumentTemplateAllSuggestion =
    { uuid : Uuid
    , name : String
    , description : String
    , version : Version
    , formats : List DocumentTemplateFormatSimple
    , organizationId : String
    , templateId : String
    }


decoder : Decoder DocumentTemplateAllSuggestion
decoder =
    D.succeed DocumentTemplateAllSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "version" Version.decoder
        |> D.required "formats" (D.list DocumentTemplateFormatSimple.decoder)
        |> D.required "organizationId" D.string
        |> D.required "templateId" D.string


compare : DocumentTemplateAllSuggestion -> DocumentTemplateAllSuggestion -> Order
compare a b =
    if a.name == b.name then
        Version.compare a.version b.version

    else
        Basics.compare a.name b.name
