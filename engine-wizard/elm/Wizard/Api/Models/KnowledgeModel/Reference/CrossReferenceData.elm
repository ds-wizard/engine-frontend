module Wizard.Api.Models.KnowledgeModel.Reference.CrossReferenceData exposing
    ( CrossReferenceData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias CrossReferenceData =
    { uuid : String
    , targetUuid : String
    , description : String
    , annotations : List Annotation
    }


decoder : Decoder CrossReferenceData
decoder =
    D.succeed CrossReferenceData
        |> D.required "uuid" D.string
        |> D.required "targetUuid" D.string
        |> D.required "description" D.string
        |> D.required "annotations" (D.list Annotation.decoder)
