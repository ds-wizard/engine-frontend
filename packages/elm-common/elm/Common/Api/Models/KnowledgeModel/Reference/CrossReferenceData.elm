module Common.Api.Models.KnowledgeModel.Reference.CrossReferenceData exposing
    ( CrossReferenceData
    , decoder
    , equalContent
    , toLabel
    )

import Common.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List


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


toLabel : List Question -> CrossReferenceData -> String
toLabel questions data =
    case List.find (\q -> Question.getUuid q == data.targetUuid) questions of
        Just question ->
            Question.getTitle question

        Nothing ->
            ""


equalContent : CrossReferenceData -> CrossReferenceData -> Bool
equalContent data1 data2 =
    (data1.targetUuid == data2.targetUuid)
        && (data1.description == data2.description)
