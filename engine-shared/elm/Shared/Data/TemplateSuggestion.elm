module Shared.Data.TemplateSuggestion exposing (TemplateSuggestion, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Version exposing (Version)


type alias TemplateSuggestion =
    { id : String
    , name : String
    , description : String
    , version : Version
    , formats : List TemplateFormat
    }


decoder : Decoder TemplateSuggestion
decoder =
    D.succeed TemplateSuggestion
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "version" Version.decoder
        |> D.required "formats" (D.list TemplateFormat.decoder)
