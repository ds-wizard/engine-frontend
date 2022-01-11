module Shared.Data.Event.AddIntegrationEventData exposing
    ( AddIntegrationEventData
    , decoder
    , encode
    , init
    , toIntegration
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Integration exposing (Integration)
import Shared.Data.KnowledgeModel.Integration.RequestHeader as RequestHeader exposing (RequestHeader)


type alias AddIntegrationEventData =
    { id : String
    , name : String
    , props : List String
    , logo : String
    , requestMethod : String
    , requestUrl : String
    , requestHeaders : List RequestHeader
    , requestBody : String
    , responseListField : String
    , responseItemId : String
    , responseItemTemplate : String
    , responseItemUrl : String
    , annotations : List Annotation
    }


decoder : Decoder AddIntegrationEventData
decoder =
    D.succeed AddIntegrationEventData
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "props" (D.list D.string)
        |> D.required "logo" D.string
        |> D.required "requestMethod" D.string
        |> D.required "requestUrl" D.string
        |> D.required "requestHeaders" (D.list RequestHeader.decoder)
        |> D.required "requestBody" D.string
        |> D.required "responseListField" D.string
        |> D.required "responseItemId" D.string
        |> D.required "responseItemTemplate" D.string
        |> D.required "responseItemUrl" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddIntegrationEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddIntegrationEvent" )
    , ( "id", E.string data.id )
    , ( "name", E.string data.name )
    , ( "props", E.list E.string data.props )
    , ( "logo", E.string data.logo )
    , ( "requestMethod", E.string data.requestMethod )
    , ( "requestUrl", E.string data.requestUrl )
    , ( "requestHeaders", E.list RequestHeader.encode data.requestHeaders )
    , ( "requestBody", E.string data.requestBody )
    , ( "responseListField", E.string data.responseListField )
    , ( "responseItemId", E.string data.responseItemId )
    , ( "responseItemTemplate", E.string data.responseItemTemplate )
    , ( "responseItemUrl", E.string data.responseItemUrl )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


init : AddIntegrationEventData
init =
    { id = ""
    , name = ""
    , props = []
    , logo = ""
    , requestMethod = ""
    , requestUrl = ""
    , requestHeaders = []
    , requestBody = ""
    , responseListField = ""
    , responseItemId = ""
    , responseItemTemplate = ""
    , responseItemUrl = ""
    , annotations = []
    }


toIntegration : String -> AddIntegrationEventData -> Integration
toIntegration uuid data =
    { uuid = uuid
    , id = data.id
    , name = data.name
    , props = data.props
    , logo = data.logo
    , requestMethod = data.requestMethod
    , requestUrl = data.requestUrl
    , requestHeaders = data.requestHeaders
    , requestBody = data.requestBody
    , responseListField = data.responseListField
    , responseItemId = data.responseItemId
    , responseItemTemplate = data.responseItemTemplate
    , responseItemUrl = data.responseItemUrl
    , annotations = data.annotations
    }
