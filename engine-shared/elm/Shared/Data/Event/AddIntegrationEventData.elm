module Shared.Data.Event.AddIntegrationEventData exposing
    ( AddIntegrationEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddIntegrationEventData =
    { id : String
    , name : String
    , props : List String
    , logo : String
    , requestMethod : String
    , requestUrl : String
    , requestHeaders : Dict String String
    , requestBody : String
    , responseListField : String
    , responseItemId : String
    , responseItemTemplate : String
    , responseItemUrl : String
    , annotations : Dict String String
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
        |> D.required "requestHeaders" (D.dict D.string)
        |> D.required "requestBody" D.string
        |> D.required "responseListField" D.string
        |> D.required "responseItemId" D.string
        |> D.required "responseItemTemplate" D.string
        |> D.required "responseItemUrl" D.string
        |> D.required "annotations" (D.dict D.string)


encode : AddIntegrationEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddIntegrationEvent" )
    , ( "id", E.string data.id )
    , ( "name", E.string data.name )
    , ( "props", E.list E.string data.props )
    , ( "logo", E.string data.logo )
    , ( "requestMethod", E.string data.requestMethod )
    , ( "requestUrl", E.string data.requestUrl )
    , ( "requestHeaders", E.dict identity E.string data.requestHeaders )
    , ( "requestBody", E.string data.requestBody )
    , ( "responseListField", E.string data.responseListField )
    , ( "responseItemId", E.string data.responseItemId )
    , ( "responseItemTemplate", E.string data.responseItemTemplate )
    , ( "responseItemUrl", E.string data.responseItemUrl )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
