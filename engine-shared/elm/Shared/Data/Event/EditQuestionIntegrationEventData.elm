module Shared.Data.Event.EditQuestionIntegrationEventData exposing
    ( EditQuestionIntegrationEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditQuestionIntegrationEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , requiredPhaseUuid : EventField (Maybe String)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , integrationUuid : EventField String
    , props : EventField (Dict String String)
    , annotations : EventField (Dict String String)
    }


decoder : Decoder EditQuestionIntegrationEventData
decoder =
    D.succeed EditQuestionIntegrationEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "requiredPhaseUuid" (EventField.decoder (D.nullable D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "referenceUuids" (EventField.decoder (D.list D.string))
        |> D.required "expertUuids" (EventField.decoder (D.list D.string))
        |> D.required "integrationUuid" (EventField.decoder D.string)
        |> D.required "props" (EventField.decoder (D.dict D.string))
        |> D.required "annotations" (EventField.decoder (D.dict D.string))


encode : EditQuestionIntegrationEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "IntegrationQuestion" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "requiredPhaseUuid", EventField.encode (E.maybe E.string) data.requiredPhaseUuid )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "referenceUuids", EventField.encode (E.list E.string) data.referenceUuids )
    , ( "expertUuids", EventField.encode (E.list E.string) data.expertUuids )
    , ( "integrationUuid", EventField.encode E.string data.integrationUuid )
    , ( "props", EventField.encode (E.dict identity E.string) data.props )
    , ( "annotations", EventField.encode (E.dict identity E.string) data.annotations )
    ]
