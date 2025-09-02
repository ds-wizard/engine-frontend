module Wizard.Api.Models.Event.AddIntegrationApiLegacyEventData exposing
    ( AddIntegrationApiLegacyEventData
    , decoder
    , encode
    , toIntegration
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration(..))
import Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair as KeyValuePair exposing (KeyValuePair)


type alias AddIntegrationApiLegacyEventData =
    { id : String
    , name : String
    , variables : List String
    , logo : Maybe String
    , itemUrl : Maybe String
    , annotations : List Annotation
    , requestMethod : String
    , requestUrl : String
    , requestHeaders : List KeyValuePair
    , requestBody : String
    , requestEmptySearch : Bool
    , responseListField : Maybe String
    , responseItemId : Maybe String
    , responseItemTemplate : String
    }


decoder : Decoder AddIntegrationApiLegacyEventData
decoder =
    D.succeed AddIntegrationApiLegacyEventData
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "variables" (D.list D.string)
        |> D.required "logo" (D.maybe D.string)
        |> D.required "itemUrl" (D.maybe D.string)
        |> D.required "annotations" (D.list Annotation.decoder)
        |> D.required "requestMethod" D.string
        |> D.required "requestUrl" D.string
        |> D.required "requestHeaders" (D.list KeyValuePair.decoder)
        |> D.required "requestBody" D.string
        |> D.required "requestEmptySearch" D.bool
        |> D.required "responseListField" (D.maybe D.string)
        |> D.required "responseItemId" (D.maybe D.string)
        |> D.required "responseItemTemplate" D.string


encode : AddIntegrationApiLegacyEventData -> List ( String, E.Value )
encode data =
    [ ( "integrationType", E.string "ApiLegacyIntegration" )
    , ( "id", E.string data.id )
    , ( "name", E.string data.name )
    , ( "variables", E.list E.string data.variables )
    , ( "logo", E.maybe E.string data.logo )
    , ( "itemUrl", E.maybe E.string data.itemUrl )
    , ( "annotations", E.list Annotation.encode data.annotations )
    , ( "requestMethod", E.string data.requestMethod )
    , ( "requestUrl", E.string data.requestUrl )
    , ( "requestHeaders", E.list KeyValuePair.encode data.requestHeaders )
    , ( "requestBody", E.string data.requestBody )
    , ( "requestEmptySearch", E.bool data.requestEmptySearch )
    , ( "responseListField", E.maybe E.string data.responseListField )
    , ( "responseItemId", E.maybe E.string data.responseItemId )
    , ( "responseItemTemplate", E.string data.responseItemTemplate )
    ]


toIntegration : String -> AddIntegrationApiLegacyEventData -> Integration
toIntegration uuid data =
    ApiLegacyIntegration
        { uuid = uuid
        , id = data.id
        , name = data.name
        , variables = data.variables
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
