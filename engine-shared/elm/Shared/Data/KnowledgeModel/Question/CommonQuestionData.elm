module Shared.Data.KnowledgeModel.Question.CommonQuestionData exposing
    ( CommonQuestionData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias CommonQuestionData =
    { uuid : String
    , title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , tagUuids : List String
    , referenceUuids : List String
    , expertUuids : List String
    , annotations : List Annotation
    }


decoder : Decoder CommonQuestionData
decoder =
    D.succeed CommonQuestionData
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "referenceUuids" (D.list D.string)
        |> D.required "expertUuids" (D.list D.string)
        |> D.required "annotations" (D.list Annotation.decoder)
