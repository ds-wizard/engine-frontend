module Shared.Data.KnowledgeModel.Reference.URLReferenceData exposing
    ( URLReferenceData
    , decoder
    , new
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias URLReferenceData =
    { uuid : String
    , url : String
    , label : String
    , annotations : Dict String String
    }


new : String -> URLReferenceData
new uuid =
    { uuid = uuid
    , url = "http://example.com"
    , label = "See also"
    , annotations = Dict.empty
    }


decoder : Decoder URLReferenceData
decoder =
    D.succeed URLReferenceData
        |> D.required "uuid" D.string
        |> D.required "url" D.string
        |> D.required "label" D.string
        |> D.required "annotations" (D.dict D.string)
