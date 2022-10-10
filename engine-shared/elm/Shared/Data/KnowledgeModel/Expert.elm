module Shared.Data.KnowledgeModel.Expert exposing (Expert, decoder, getVisibleName)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias Expert =
    { uuid : String
    , name : String
    , email : String
    , annotations : List Annotation
    }


decoder : Decoder Expert
decoder =
    D.succeed Expert
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "email" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


getVisibleName : Expert -> String
getVisibleName expert =
    if String.isEmpty expert.name then
        expert.email

    else
        expert.name
