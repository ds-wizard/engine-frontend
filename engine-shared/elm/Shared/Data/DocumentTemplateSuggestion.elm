module Shared.Data.DocumentTemplateSuggestion exposing
    ( DocumentTemplateSuggestion
    , createOptions
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Shared.Data.DocumentTemplate.DocumentTemplateFormatSimple as DocumentTemplateFormatSimple exposing (DocumentTemplateFormatSimple)
import Shared.Utils exposing (getOrganizationAndItemId)
import Version exposing (Version)


type alias DocumentTemplateSuggestion =
    { id : String
    , name : String
    , description : String
    , version : Version
    , formats : List DocumentTemplateFormatSimple
    }


decoder : Decoder DocumentTemplateSuggestion
decoder =
    D.succeed DocumentTemplateSuggestion
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "version" Version.decoder
        |> D.required "formats" (D.list DocumentTemplateFormatSimple.decoder)


createOptions : List DocumentTemplateSuggestion -> List ( String, String )
createOptions templates =
    templates
        |> List.map (.id >> getOrganizationAndItemId)
        |> List.unique
        |> List.sort
        |> List.map (\t -> ( t, t ))
        |> (::) ( "", "--" )
