module Shared.Data.KnowledgeModel.Reference.CrossReferenceData exposing
    ( CrossReferenceData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias CrossReferenceData =
    { uuid : String
    , targetUuid : String
    , description : String
    }


decoder : Decoder CrossReferenceData
decoder =
    D.succeed CrossReferenceData
        |> D.required "uuid" D.string
        |> D.required "targetUuid" D.string
        |> D.required "description" D.string
