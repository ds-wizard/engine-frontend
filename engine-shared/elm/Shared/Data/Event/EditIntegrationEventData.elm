module Shared.Data.Event.EditIntegrationEventData exposing
    ( EditIntegrationEventData
    , apply
    , decoder
    , encode
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Integration exposing (Integration)
import Shared.Data.KnowledgeModel.Integration.RequestHeader as RequestHeader exposing (RequestHeader)


type alias EditIntegrationEventData =
    { id : EventField String
    , name : EventField String
    , props : EventField (List String)
    , logo : EventField String
    , requestMethod : EventField String
    , requestUrl : EventField String
    , requestHeaders : EventField (List RequestHeader)
    , requestBody : EventField String
    , responseListField : EventField String
    , responseItemId : EventField String
    , responseItemTemplate : EventField String
    , responseItemUrl : EventField String
    , annotations : EventField (List Annotation)
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
        |> D.required "requestHeaders" (EventField.decoder (D.list RequestHeader.decoder))
        |> D.required "requestBody" (EventField.decoder D.string)
        |> D.required "responseListField" (EventField.decoder D.string)
        |> D.required "responseItemId" (EventField.decoder D.string)
        |> D.required "responseItemTemplate" (EventField.decoder D.string)
        |> D.required "responseItemUrl" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditIntegrationEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditIntegrationEvent" )
    , ( "id", EventField.encode E.string data.id )
    , ( "name", EventField.encode E.string data.name )
    , ( "props", EventField.encode (E.list E.string) data.props )
    , ( "logo", EventField.encode E.string data.logo )
    , ( "requestMethod", EventField.encode E.string data.requestMethod )
    , ( "requestUrl", EventField.encode E.string data.requestUrl )
    , ( "requestHeaders", EventField.encode (E.list RequestHeader.encode) data.requestHeaders )
    , ( "requestBody", EventField.encode E.string data.requestBody )
    , ( "responseListField", EventField.encode E.string data.responseListField )
    , ( "responseItemId", EventField.encode E.string data.responseItemId )
    , ( "responseItemTemplate", EventField.encode E.string data.responseItemTemplate )
    , ( "responseItemUrl", EventField.encode E.string data.responseItemUrl )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditIntegrationEventData
init =
    { id = EventField.empty
    , name = EventField.empty
    , props = EventField.empty
    , logo = EventField.empty
    , requestMethod = EventField.empty
    , requestUrl = EventField.empty
    , requestHeaders = EventField.empty
    , requestBody = EventField.empty
    , responseListField = EventField.empty
    , responseItemId = EventField.empty
    , responseItemTemplate = EventField.empty
    , responseItemUrl = EventField.empty
    , annotations = EventField.empty
    }


apply : EditIntegrationEventData -> Integration -> Integration
apply eventData integration =
    { integration
        | id = EventField.getValueWithDefault eventData.id integration.id
        , name = EventField.getValueWithDefault eventData.name integration.name
        , props = EventField.getValueWithDefault eventData.props integration.props
        , logo = EventField.getValueWithDefault eventData.logo integration.logo
        , requestMethod = EventField.getValueWithDefault eventData.requestMethod integration.requestMethod
        , requestUrl = EventField.getValueWithDefault eventData.requestUrl integration.requestUrl
        , requestHeaders = EventField.getValueWithDefault eventData.requestHeaders integration.requestHeaders
        , requestBody = EventField.getValueWithDefault eventData.requestBody integration.requestBody
        , responseListField = EventField.getValueWithDefault eventData.responseListField integration.responseListField
        , responseItemId = EventField.getValueWithDefault eventData.responseItemId integration.responseItemId
        , responseItemTemplate = EventField.getValueWithDefault eventData.responseItemTemplate integration.responseItemTemplate
        , responseItemUrl = EventField.getValueWithDefault eventData.responseItemUrl integration.responseItemUrl
        , annotations = EventField.getValueWithDefault eventData.annotations integration.annotations
    }
