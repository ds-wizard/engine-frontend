module Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric, decoder, equalContent)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


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


equalContent : Metric -> Metric -> Bool
equalContent metric1 metric2 =
    (metric1.title == metric2.title)
        && (metric1.abbreviation == metric2.abbreviation)
        && (metric1.description == metric2.description)
