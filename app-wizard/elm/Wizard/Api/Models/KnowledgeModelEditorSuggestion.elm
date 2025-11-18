module Wizard.Api.Models.KnowledgeModelEditorSuggestion exposing
    ( KnowledgeModelEditorSuggestion
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias KnowledgeModelEditorSuggestion =
    { uuid : Uuid
    , name : String
    }


decoder : Decoder KnowledgeModelEditorSuggestion
decoder =
    D.succeed KnowledgeModelEditorSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
