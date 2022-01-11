module Shared.Data.KnowledgeModel.Metric exposing (Metric, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias Metric =
    { uuid : String
    , title : String
    , abbreviation : Maybe String
    , description : Maybe String
    , annotations : List Annotation
    }


decoder : Decoder Metric
decoder =
    D.succeed Metric
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "abbreviation" (D.maybe D.string)
        |> D.required "description" (D.maybe D.string)
        |> D.required "annotations" (D.list Annotation.decoder)
