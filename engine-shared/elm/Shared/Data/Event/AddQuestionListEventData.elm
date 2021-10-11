module Shared.Data.Event.AddQuestionListEventData exposing
    ( AddQuestionListEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AddQuestionListEventData =
    { title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , tagUuids : List String
    , annotations : Dict String String
    }


decoder : Decoder AddQuestionListEventData
decoder =
    D.succeed AddQuestionListEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "annotations" (D.dict D.string)


encode : AddQuestionListEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "ListQuestion" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "requiredPhaseUuid", E.maybe E.string data.requiredPhaseUuid )
    , ( "tagUuids", E.list E.string data.tagUuids )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
