module Shared.Data.KnowledgeModel.Chapter exposing
    ( Chapter
    , addQuestionUuid
    , decoder
    , removeQuestionUuid
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias Chapter =
    { uuid : String
    , title : String
    , text : Maybe String
    , questionUuids : List String
    , annotations : List Annotation
    }


decoder : Decoder Chapter
decoder =
    D.succeed Chapter
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "questionUuids" (D.list D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


addQuestionUuid : String -> Chapter -> Chapter
addQuestionUuid questionUuid chapter =
    { chapter | questionUuids = chapter.questionUuids ++ [ questionUuid ] }


removeQuestionUuid : String -> Chapter -> Chapter
removeQuestionUuid questionUuid chapter =
    { chapter | questionUuids = List.filter ((/=) questionUuid) chapter.questionUuids }
