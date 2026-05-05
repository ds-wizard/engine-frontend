module Wizard.Api.Models.KnowledgeModel.Reference.URLReferenceData exposing
    ( URLReferenceData
    , decoder
    , equalContent
    , toLabel
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias URLReferenceData =
    { uuid : String
    , url : String
    , label : String
    , annotations : List Annotation
    }


decoder : Decoder URLReferenceData
decoder =
    D.succeed URLReferenceData
        |> D.required "uuid" D.string
        |> D.required "url" D.string
        |> D.required "label" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


toLabel : URLReferenceData -> String
toLabel data =
    if String.isEmpty data.label then
        data.url

    else
        data.label


equalContent : URLReferenceData -> URLReferenceData -> Bool
equalContent data1 data2 =
    (data1.url == data2.url)
        && (data1.label == data2.label)
