module Wizard.Api.Models.KnowledgeModel.Expert exposing (Expert, decoder, equalContent, getVisibleName)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


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


equalContent : Expert -> Expert -> Bool
equalContent expert1 expert2 =
    (expert1.name == expert2.name)
        && (expert1.email == expert2.email)
