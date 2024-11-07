module Shared.Data.Event.AddQuestionFileEventData exposing
    ( AddQuestionFileEventData
    , decoder
    , encode
    , toQuestion
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Question exposing (Question(..))


type alias AddQuestionFileEventData =
    { title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , maxSize : Maybe Int
    , fileTypes : Maybe String
    , tagUuids : List String
    , annotations : List Annotation
    }


decoder : Decoder AddQuestionFileEventData
decoder =
    D.succeed AddQuestionFileEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "maxSize" (D.maybe D.int)
        |> D.required "fileTypes" (D.maybe D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddQuestionFileEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "FileQuestion" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "requiredPhaseUuid", E.maybe E.string data.requiredPhaseUuid )
    , ( "maxSize", E.maybe E.int data.maxSize )
    , ( "fileTypes", E.maybe E.string data.fileTypes )
    , ( "tagUuids", E.list E.string data.tagUuids )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


toQuestion : String -> AddQuestionFileEventData -> Question
toQuestion uuid data =
    FileQuestion
        { uuid = uuid
        , title = data.title
        , text = data.text
        , requiredPhaseUuid = data.requiredPhaseUuid
        , tagUuids = data.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , annotations = data.annotations
        }
        { maxSize = data.maxSize
        , fileTypes = data.fileTypes
        }
