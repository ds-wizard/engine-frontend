module Shared.Data.Event.AddQuestionIntegrationEventData exposing
    ( AddQuestionIntegrationEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AddQuestionIntegrationEventData =
    { title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , tagUuids : List String
    , integrationUuid : String
    , props : Dict String String
    }


decoder : Decoder AddQuestionIntegrationEventData
decoder =
    D.succeed AddQuestionIntegrationEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredLevel" (D.nullable D.int)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "integrationUuid" D.string
        |> D.required "props" (D.dict D.string)


encode : AddQuestionIntegrationEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "IntegrationQuestion" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "requiredLevel", E.maybe E.int data.requiredLevel )
    , ( "tagUuids", E.list E.string data.tagUuids )
    , ( "integrationUuid", E.string data.integrationUuid )
    , ( "props", E.dict identity E.string data.props )
    ]
