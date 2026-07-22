module Wizard.Api.Models.KnowledgeModelLocale exposing
    ( KnowledgeModelLocale
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias KnowledgeModelLocale =
    { uuid : Uuid
    , code : String
    , name : String
    }


decoder : Decoder KnowledgeModelLocale
decoder =
    D.succeed KnowledgeModelLocale
        |> D.required "uuid" Uuid.decoder
        |> D.required "code" D.string
        |> D.required "name" D.string
