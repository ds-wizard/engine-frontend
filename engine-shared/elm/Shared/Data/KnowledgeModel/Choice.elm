module Shared.Data.KnowledgeModel.Choice exposing
    ( Choice
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias Choice =
    { uuid : String
    , label : String
    , annotations : List Annotation
    }


decoder : Decoder Choice
decoder =
    D.succeed Choice
        |> D.required "uuid" D.string
        |> D.required "label" D.string
        |> D.required "annotations" (D.list Annotation.decoder)
