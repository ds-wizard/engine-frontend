module Shared.Data.Event.AddQuestionValueEventData exposing
    ( AddQuestionValueEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.KnowledgeModel.Question.QuestionValueType as QuestionValueType exposing (QuestionValueType)


type alias AddQuestionValueEventData =
    { title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , tagUuids : List String
    , valueType : QuestionValueType
    }


decoder : Decoder AddQuestionValueEventData
decoder =
    D.succeed AddQuestionValueEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "valueType" QuestionValueType.decoder


encode : AddQuestionValueEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "ValueQuestion" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "requiredPhaseUuid", E.maybe E.string data.requiredPhaseUuid )
    , ( "tagUuids", E.list E.string data.tagUuids )
    , ( "valueType", QuestionValueType.encode data.valueType )
    ]
