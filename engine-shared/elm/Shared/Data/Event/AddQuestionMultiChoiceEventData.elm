module Shared.Data.Event.AddQuestionMultiChoiceEventData exposing (AddQuestionMultiChoiceEventData, decoder, encode)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AddQuestionMultiChoiceEventData =
    { title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , tagUuids : List String
    , annotations : Dict String String
    }


decoder : Decoder AddQuestionMultiChoiceEventData
decoder =
    D.succeed AddQuestionMultiChoiceEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "annotations" (D.dict D.string)


encode : AddQuestionMultiChoiceEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "MultiChoiceQuestion" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "requiredPhaseUuid", E.maybe E.string data.requiredPhaseUuid )
    , ( "tagUuids", E.list E.string data.tagUuids )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
