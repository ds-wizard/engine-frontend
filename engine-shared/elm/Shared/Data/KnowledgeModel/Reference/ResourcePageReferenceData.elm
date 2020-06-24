module Shared.Data.KnowledgeModel.Reference.ResourcePageReferenceData exposing
    ( ResourcePageReferenceData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias ResourcePageReferenceData =
    { uuid : String
    , shortUuid : String
    }


decoder : Decoder ResourcePageReferenceData
decoder =
    D.succeed ResourcePageReferenceData
        |> D.required "uuid" D.string
        |> D.required "shortUuid" D.string
