module Wizard.Api.Models.DocumentTemplateAllSuggestion exposing
    ( DocumentTemplateAllSuggestion
    , createOptions
    , decoder
    , getOrganizationAndTemplateId
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
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


getOrganizationAndTemplateId : DocumentTemplateAllSuggestion -> String
getOrganizationAndTemplateId template =
    template.organizationId ++ ":" ++ template.templateId


createOptions : List DocumentTemplateAllSuggestion -> List ( String, String )
createOptions templates =
    templates
        |> List.map getOrganizationAndTemplateId
        |> List.unique
        |> List.sort
        |> List.map (\t -> ( t, t ))
        |> (::) ( "", "--" )
