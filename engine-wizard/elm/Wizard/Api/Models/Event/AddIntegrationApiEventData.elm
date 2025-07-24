module Wizard.Api.Models.Event.AddIntegrationApiEventData exposing
    ( AddIntegrationApiEventData
    , decoder
    , encode
    , init
    , toIntegration
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration(..))
import Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair as KeyValuePair exposing (KeyValuePair)
import Wizard.Api.Models.TypeHintTestResponse as TypeHintTestResponse exposing (TypeHintTestResponse)


type alias AddIntegrationApiEventData =
    { allowCustomReply : Bool
    , annotations : List Annotation
    , name : String
    , requestAllowEmptySearch : Bool
    , requestBody : Maybe String
    , requestHeaders : List KeyValuePair
    , requestMethod : String
    , requestUrl : String
    , responseItemTemplate : String
    , responseItemTemplateForSelection : Maybe String
    , responseListField : Maybe String
    , testQ : String
    , testResponse : Maybe TypeHintTestResponse
    , testVariables : Dict String String
    , variables : List String
    }


decoder : Decoder AddIntegrationApiEventData
decoder =
    D.succeed AddIntegrationApiEventData
        |> D.required "allowCustomReply" D.bool
        |> D.required "annotations" (D.list Annotation.decoder)
        |> D.required "name" D.string
        |> D.required "requestAllowEmptySearch" D.bool
        |> D.required "requestBody" (D.maybe D.string)
        |> D.required "requestHeaders" (D.list KeyValuePair.decoder)
        |> D.required "requestMethod" D.string
        |> D.required "requestUrl" D.string
        |> D.required "responseItemTemplate" D.string
        |> D.required "responseItemTemplateForSelection" (D.maybe D.string)
        |> D.required "responseListField" (D.maybe D.string)
        |> D.required "testQ" D.string
        |> D.required "testResponse" (D.maybe TypeHintTestResponse.decoder)
        |> D.required "testVariables" (D.dict D.string)
        |> D.required "variables" (D.list D.string)


encode : AddIntegrationApiEventData -> List ( String, E.Value )
encode data =
    [ ( "integrationType", E.string "ApiIntegration" )
    , ( "allowCustomReply", E.bool data.allowCustomReply )
    , ( "annotations", E.list Annotation.encode data.annotations )
    , ( "name", E.string data.name )
    , ( "requestAllowEmptySearch", E.bool data.requestAllowEmptySearch )
    , ( "requestBody", E.maybe E.string data.requestBody )
    , ( "requestHeaders", E.list KeyValuePair.encode data.requestHeaders )
    , ( "requestMethod", E.string data.requestMethod )
    , ( "requestUrl", E.string data.requestUrl )
    , ( "responseItemTemplate", E.string data.responseItemTemplate )
    , ( "responseItemTemplateForSelection", E.maybe E.string data.responseItemTemplateForSelection )
    , ( "responseListField", E.maybe E.string data.responseListField )
    , ( "testQ", E.string data.testQ )
    , ( "testResponse", E.maybe TypeHintTestResponse.encode data.testResponse )
    , ( "testVariables", E.dict identity E.string data.testVariables )
    , ( "variables", E.list E.string data.variables )
    ]


init : AddIntegrationApiEventData
init =
    { allowCustomReply = True
    , annotations = []
    , name = ""
    , requestAllowEmptySearch = True
    , requestBody = Nothing
    , requestHeaders = [ { key = "Accept", value = "application/json" } ]
    , requestMethod = "GET"
    , requestUrl = ""
    , responseItemTemplate = ""
    , responseItemTemplateForSelection = Nothing
    , responseListField = Nothing
    , testQ = ""
    , testResponse = Nothing
    , testVariables = Dict.empty
    , variables = []
    }


toIntegration : String -> AddIntegrationApiEventData -> Integration
toIntegration uuid data =
    ApiIntegration
        { allowCustomReply = data.allowCustomReply
        , annotations = data.annotations
        , name = data.name
        , requestAllowEmptySearch = data.requestAllowEmptySearch
        , requestBody = data.requestBody
        , requestHeaders = data.requestHeaders
        , requestMethod = data.requestMethod
        , requestUrl = data.requestUrl
        , responseItemTemplate = data.responseItemTemplate
        , responseItemTemplateForSelection = data.responseItemTemplateForSelection
        , responseListField = data.responseListField
        , testQ = data.testQ
        , testResponse = data.testResponse
        , testVariables = data.testVariables
        , uuid = uuid
        , variables = data.variables
        }
