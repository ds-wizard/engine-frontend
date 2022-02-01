module Shared.Data.KnowledgeModel.Reference.ResourcePageReferenceData exposing
    ( ResourcePageReferenceData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias ResourcePageReferenceData =
    { uuid : String
    , shortUuid : String
    , annotations : List Annotation
    }


decoder : Decoder ResourcePageReferenceData
decoder =
    D.succeed ResourcePageReferenceData
        |> D.required "uuid" D.string
        |> D.required "shortUuid" D.string
        |> D.required "annotations" (D.list Annotation.decoder)
