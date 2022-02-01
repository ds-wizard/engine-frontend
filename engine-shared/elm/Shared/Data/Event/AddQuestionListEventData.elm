module Shared.Data.Event.AddQuestionListEventData exposing
    ( AddQuestionListEventData
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


type alias AddQuestionListEventData =
    { title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , tagUuids : List String
    , annotations : List Annotation
    }


decoder : Decoder AddQuestionListEventData
decoder =
    D.succeed AddQuestionListEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddQuestionListEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "ListQuestion" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "requiredPhaseUuid", E.maybe E.string data.requiredPhaseUuid )
    , ( "tagUuids", E.list E.string data.tagUuids )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


toQuestion : String -> AddQuestionListEventData -> Question
toQuestion uuid data =
    ListQuestion
        { uuid = uuid
        , title = data.title
        , text = data.text
        , requiredPhaseUuid = data.requiredPhaseUuid
        , tagUuids = data.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , annotations = data.annotations
        }
        { itemTemplateQuestionUuids = []
        }
