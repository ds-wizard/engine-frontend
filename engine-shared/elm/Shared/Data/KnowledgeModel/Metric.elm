module Shared.Data.KnowledgeModel.Metric exposing (Metric, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Metric =
    { uuid : String
    , title : String
    , abbreviation : String
    , description : String
    }


decoder : Decoder Metric
decoder =
    D.succeed Metric
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "abbreviation" D.string
        |> D.required "description" D.string
