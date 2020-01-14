module Wizard.KMEditor.Common.Events.AddQuestionListEventData exposing
    ( AddQuestionListEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AddQuestionListEventData =
    { title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , tagUuids : List String
    }


decoder : Decoder AddQuestionListEventData
decoder =
    D.succeed AddQuestionListEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredLevel" (D.nullable D.int)
        |> D.required "tagUuids" (D.list D.string)


encode : AddQuestionListEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "ListQuestion" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "requiredLevel", E.maybe E.int data.requiredLevel )
    , ( "tagUuids", E.list E.string data.tagUuids )
    ]
