module Wizard.Api.Models.Event.AddIntegrationApiEventData exposing
    ( AddIntegrationApiEventData
    , decoder
    , encode
    , init
    , toIntegration
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration(..))
import Wizard.Api.Models.KnowledgeModel.Integration.RequestHeader as RequestHeader exposing (RequestHeader)


type alias AddIntegrationApiEventData =
    { id : String
    , name : String
    , props : List String
    , logo : Maybe String
    , itemUrl : Maybe String
    , annotations : List Annotation
    , requestMethod : String
    , requestUrl : String
    , requestHeaders : List RequestHeader
    , requestBody : String
    , requestEmptySearch : Bool
    , responseListField : Maybe String
    , responseItemId : Maybe String
    , responseItemTemplate : String
    }


decoder : Decoder AddIntegrationApiEventData
decoder =
    D.succeed AddIntegrationApiEventData
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "props" (D.list D.string)
        |> D.required "logo" (D.maybe D.string)
        |> D.required "itemUrl" (D.maybe D.string)
        |> D.required "annotations" (D.list Annotation.decoder)
        |> D.required "requestMethod" D.string
        |> D.required "requestUrl" D.string
        |> D.required "requestHeaders" (D.list RequestHeader.decoder)
        |> D.required "requestBody" D.string
        |> D.required "requestEmptySearch" D.bool
        |> D.required "responseListField" (D.maybe D.string)
        |> D.required "responseItemId" (D.maybe D.string)
        |> D.required "responseItemTemplate" D.string


encode : AddIntegrationApiEventData -> List ( String, E.Value )
encode data =
    [ ( "integrationType", E.string "ApiIntegration" )
    , ( "id", E.string data.id )
    , ( "name", E.string data.name )
    , ( "props", E.list E.string data.props )
    , ( "logo", E.maybe E.string data.logo )
    , ( "itemUrl", E.maybe E.string data.itemUrl )
    , ( "annotations", E.list Annotation.encode data.annotations )
    , ( "requestMethod", E.string data.requestMethod )
    , ( "requestUrl", E.string data.requestUrl )
    , ( "requestHeaders", E.list RequestHeader.encode data.requestHeaders )
    , ( "requestBody", E.string data.requestBody )
    , ( "requestEmptySearch", E.bool data.requestEmptySearch )
    , ( "responseListField", E.maybe E.string data.responseListField )
    , ( "responseItemId", E.maybe E.string data.responseItemId )
    , ( "responseItemTemplate", E.string data.responseItemTemplate )
    ]


init : AddIntegrationApiEventData
init =
    { id = ""
    , name = ""
    , props = []
    , logo = Nothing
    , itemUrl = Nothing
    , annotations = []
    , requestMethod = ""
    , requestUrl = ""
    , requestHeaders = []
    , requestBody = ""
    , requestEmptySearch = True
    , responseListField = Nothing
    , responseItemId = Nothing
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
