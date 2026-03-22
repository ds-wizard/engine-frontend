module Wizard.Api.Models.KnowledgeModel.Integration exposing
    ( Integration(..)
    , decoder
    , getAllowCustomReply
    , getAnnotations
    , getName
    , getRequestAllowEmptySearch
    , getRequestBody
    , getRequestHeaders
    , getRequestMethod
    , getRequestUrl
    , getResponseItemTemplate
    , getResponseItemTemplateForSelection
    , getResponseListField
    , getTestQ
    , getTestResponse
    , getTestVariables
    , getTypeString
    , getUuid
    , getVariables
    , getVisibleName
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Wizard.Api.Models.KnowledgeModel.Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Integration.ApiIntegrationData as ApiIntegrationData exposing (ApiIntegrationData)
import Wizard.Api.Models.KnowledgeModel.Integration.IntegrationType as IntegrationType
import Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair exposing (KeyValuePair)
import Wizard.Api.Models.TypeHintTestResponse exposing (TypeHintTestResponse)


type Integration
    = ApiIntegration ApiIntegrationData



-- Decoders


decoder : Decoder Integration
decoder =
    --D.oneOf
    --    [ D.when IntegrationType.decoder ((==) IntegrationType.Api) apiIntegrationDecoder
    --    ]
    D.when IntegrationType.decoder ((==) IntegrationType.Api) apiIntegrationDecoder


apiIntegrationDecoder : Decoder Integration
apiIntegrationDecoder =
    D.map ApiIntegration ApiIntegrationData.decoder



-- Helpers


getTypeString : Integration -> String
getTypeString integration =
    case integration of
        ApiIntegration _ ->
            "Api"


getUuid : Integration -> String
getUuid integration =
    case integration of
        ApiIntegration data ->
            data.uuid


getName : Integration -> String
getName integration =
    case integration of
        ApiIntegration data ->
            data.name


getVisibleName : Integration -> String
getVisibleName integration =
    case integration of
        ApiIntegration data ->
            data.name


getAnnotations : Integration -> List Annotation
getAnnotations integration =
    case integration of
        ApiIntegration data ->
            data.annotations


getAllowCustomReply : Integration -> Maybe Bool
getAllowCustomReply =
    getApiIntegrationData (Just << .allowCustomReply)


getRequestAllowEmptySearch : Integration -> Maybe Bool
getRequestAllowEmptySearch =
    getApiIntegrationData (Just << .requestAllowEmptySearch)


getRequestBody : Integration -> Maybe String
getRequestBody integration =
    getApiIntegrationData .requestBody integration


getRequestHeaders : Integration -> Maybe (List KeyValuePair)
getRequestHeaders integration =
    getApiIntegrationData (Just << .requestHeaders) integration


getRequestMethod : Integration -> Maybe String
getRequestMethod integration =
    getApiIntegrationData (Just << .requestMethod) integration


getRequestUrl : Integration -> Maybe String
getRequestUrl integration =
    getApiIntegrationData (Just << .requestUrl) integration


getResponseItemTemplate : Integration -> Maybe String
getResponseItemTemplate integration =
    getApiIntegrationData (Just << .responseItemTemplate) integration


getResponseItemTemplateForSelection : Integration -> Maybe String
getResponseItemTemplateForSelection =
    getApiIntegrationData .responseItemTemplateForSelection


getResponseListField : Integration -> Maybe String
getResponseListField integration =
    getApiIntegrationData .responseListField integration


getTestQ : Integration -> Maybe String
getTestQ =
    getApiIntegrationData (Just << .testQ)


getTestResponse : Integration -> Maybe TypeHintTestResponse
getTestResponse =
    getApiIntegrationData .testResponse


getTestVariables : Integration -> Maybe (Dict String String)
getTestVariables =
    getApiIntegrationData (Just << .testVariables)


getVariables : Integration -> List String
getVariables integration =
    case integration of
        ApiIntegration data ->
            data.variables


getApiIntegrationData : (ApiIntegrationData -> Maybe a) -> Integration -> Maybe a
getApiIntegrationData map integration =
    case integration of
        ApiIntegration data ->
            map data
