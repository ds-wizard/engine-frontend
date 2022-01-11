module Shared.Data.KnowledgeModel.Phase exposing (Phase, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Uuid


type alias Phase =
    { uuid : String
    , title : String
    , description : Maybe String
    , annotations : List Annotation
    }


decoder : Decoder Phase
decoder =
    D.succeed Phase
        |> D.optional "uuid" D.string (Uuid.toString Uuid.nil)
        |> D.required "title" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "annotations" (D.list Annotation.decoder)
