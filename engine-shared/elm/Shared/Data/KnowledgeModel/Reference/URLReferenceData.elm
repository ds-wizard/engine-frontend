module Shared.Data.KnowledgeModel.Reference.URLReferenceData exposing
    ( URLReferenceData
    , decoder
    , new
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias URLReferenceData =
    { uuid : String
    , url : String
    , label : String
    , annotations : List Annotation
    }


new : String -> URLReferenceData
new uuid =
    { uuid = uuid
    , url = "http://example.com"
    , label = "See also"
    , annotations = []
    }


decoder : Decoder URLReferenceData
decoder =
    D.succeed URLReferenceData
        |> D.required "uuid" D.string
        |> D.required "url" D.string
        |> D.required "label" D.string
        |> D.required "annotations" (D.list Annotation.decoder)
