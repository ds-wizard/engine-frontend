module KMEditor.Common.Events.EditIntegrationEventData exposing
    ( EditIntegrationEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import KMEditor.Common.Events.EventField as EventField exposing (EventField)


type alias EditIntegrationEventData =
    { id : EventField String
    , name : EventField String
    , props : EventField (List String)
    , logo : EventField String
    , requestMethod : EventField String
    , requestUrl : EventField String
    , requestHeaders : EventField (Dict String String)
    , requestBody : EventField String
    , responseListField : EventField String
    , responseIdField : EventField String
    , responseNameField : EventField String
    , itemUrl : EventField String
    }


decoder : Decoder EditIntegrationEventData
decoder =
    D.succeed EditIntegrationEventData
        |> D.required "id" (EventField.decoder D.string)
        |> D.required "name" (EventField.decoder D.string)
        |> D.required "props" (EventField.decoder (D.list D.string))
        |> D.required "logo" (EventField.decoder D.string)
        |> D.required "requestMethod" (EventField.decoder D.string)
        |> D.required "requestUrl" (EventField.decoder D.string)
        |> D.required "requestHeaders" (EventField.decoder (D.dict D.string))
        |> D.required "requestBody" (EventField.decoder D.string)
        |> D.required "responseListField" (EventField.decoder D.string)
        |> D.required "responseIdField" (EventField.decoder D.string)
        |> D.required "responseNameField" (EventField.decoder D.string)
        |> D.required "itemUrl" (EventField.decoder D.string)


encode : EditIntegrationEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditIntegrationEvent" )
    , ( "id", EventField.encode E.string data.id )
    , ( "name", EventField.encode E.string data.name )
    , ( "props", EventField.encode (E.list E.string) data.props )
    , ( "logo", EventField.encode E.string data.logo )
    , ( "requestMethod", EventField.encode E.string data.requestMethod )
    , ( "requestUrl", EventField.encode E.string data.requestUrl )
    , ( "requestHeaders", EventField.encode (E.dict identity E.string) data.requestHeaders )
    , ( "requestBody", EventField.encode E.string data.requestBody )
    , ( "responseListField", EventField.encode E.string data.responseListField )
    , ( "responseIdField", EventField.encode E.string data.responseIdField )
    , ( "responseNameField", EventField.encode E.string data.responseNameField )
    , ( "itemUrl", EventField.encode E.string data.itemUrl )
    ]
