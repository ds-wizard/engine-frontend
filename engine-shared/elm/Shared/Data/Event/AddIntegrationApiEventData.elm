module Shared.Data.Event.AddIntegrationApiEventData exposing
    ( AddIntegrationApiEventData
    , decoder
    , encode
    , init
    , toIntegration
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Integration exposing (Integration(..))
import Shared.Data.KnowledgeModel.Integration.RequestHeader as RequestHeader exposing (RequestHeader)


type alias AddIntegrationApiEventData =
    { id : String
    , name : String
    , props : List String
    , logo : String
    , itemUrl : String
    , annotations : List Annotation
    , requestMethod : String
    , requestUrl : String
    , requestHeaders : List RequestHeader
    , requestBody : String
    , requestEmptySearch : Bool
    , responseListField : String
    , responseItemId : String
    , responseItemTemplate : String
    }


decoder : Decoder AddIntegrationApiEventData
decoder =
    D.succeed AddIntegrationApiEventData
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "props" (D.list D.string)
        |> D.required "logo" D.string
        |> D.required "itemUrl" D.string
        |> D.required "annotations" (D.list Annotation.decoder)
        |> D.required "requestMethod" D.string
        |> D.required "requestUrl" D.string
        |> D.required "requestHeaders" (D.list RequestHeader.decoder)
        |> D.required "requestBody" D.string
        |> D.required "requestEmptySearch" D.bool
        |> D.required "responseListField" D.string
        |> D.required "responseItemId" D.string
        |> D.required "responseItemTemplate" D.string


encode : AddIntegrationApiEventData -> List ( String, E.Value )
encode data =
    [ ( "integrationType", E.string "ApiIntegration" )
    , ( "id", E.string data.id )
    , ( "name", E.string data.name )
    , ( "props", E.list E.string data.props )
    , ( "logo", E.string data.logo )
    , ( "itemUrl", E.string data.itemUrl )
    , ( "annotations", E.list Annotation.encode data.annotations )
    , ( "requestMethod", E.string data.requestMethod )
    , ( "requestUrl", E.string data.requestUrl )
    , ( "requestHeaders", E.list RequestHeader.encode data.requestHeaders )
    , ( "requestBody", E.string data.requestBody )
    , ( "requestEmptySearch", E.bool data.requestEmptySearch )
    , ( "responseListField", E.string data.responseListField )
    , ( "responseItemId", E.string data.responseItemId )
    , ( "responseItemTemplate", E.string data.responseItemTemplate )
    ]


init : AddIntegrationApiEventData
init =
    { id = ""
    , name = ""
    , props = []
    , logo = ""
    , itemUrl = ""
    , annotations = []
    , requestMethod = ""
    , requestUrl = ""
    , requestHeaders = []
    , requestBody = ""
    , requestEmptySearch = True
    , responseListField = ""
    , responseItemId = ""
    , responseItemTemplate = ""
    }


toIntegration : String -> AddIntegrationApiEventData -> Integration
toIntegration uuid data =
    ApiIntegration
        { uuid = uuid
        , id = data.id
        , name = data.name
        , props = data.props
        , logo = data.logo
        , itemUrl = data.itemUrl
        , annotations = data.annotations
        }
        { requestMethod = data.requestMethod
        , requestUrl = data.requestUrl
        , requestHeaders = data.requestHeaders
        , requestBody = data.requestBody
        , requestEmptySearch = data.requestEmptySearch
        , responseListField = data.responseListField
        , responseItemId = data.responseItemId
        , responseItemTemplate = data.responseItemTemplate
        }
