module Wizard.Api.Models.KnowledgeModel.Integration exposing
    ( Integration(..)
    , decoder
    , getAllowCustomReply
    , getAnnotations
    , getId
    , getItemUrl
    , getLogo
    , getName
    , getRequestAllowEmptySearch
    , getRequestBody
    , getRequestEmptySearch
    , getRequestHeaders
    , getRequestMethod
    , getRequestUrl
    , getResponseItemId
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
    , getWidgetUrl
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Maybe.Extra as Maybe
import Wizard.Api.Models.KnowledgeModel.Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Integration.ApiIntegrationData as ApiIntegrationData exposing (ApiIntegrationData)
import Wizard.Api.Models.KnowledgeModel.Integration.ApiLegacyIntegrationData as ApiLegacyIntegrationData exposing (ApiLegacyIntegrationData)
import Wizard.Api.Models.KnowledgeModel.Integration.CommonIntegrationData as CommonIntegrationData exposing (CommonIntegrationData)
import Wizard.Api.Models.KnowledgeModel.Integration.IntegrationType as IntegrationType
import Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair exposing (KeyValuePair)
import Wizard.Api.Models.KnowledgeModel.Integration.WidgetIntegrationData as WidgetIntegrationData exposing (WidgetIntegrationData)
import Wizard.Api.Models.TypeHintTestResponse exposing (TypeHintTestResponse)


type Integration
    = ApiIntegration ApiIntegrationData
    | ApiLegacyIntegration CommonIntegrationData ApiLegacyIntegrationData
    | WidgetIntegration CommonIntegrationData WidgetIntegrationData



-- Decoders


decoder : Decoder Integration
decoder =
    D.oneOf
        [ D.when IntegrationType.decoder ((==) IntegrationType.Api) apiIntegrationDecoder
        , D.when IntegrationType.decoder ((==) IntegrationType.ApiLegacy) apiLegacyIntegrationDecoder
        , D.when IntegrationType.decoder ((==) IntegrationType.Widget) widgetIntegrationDecoder
        ]


apiIntegrationDecoder : Decoder Integration
apiIntegrationDecoder =
    D.map ApiIntegration ApiIntegrationData.decoder


apiLegacyIntegrationDecoder : Decoder Integration
apiLegacyIntegrationDecoder =
    D.map2 ApiLegacyIntegration CommonIntegrationData.decoder ApiLegacyIntegrationData.decoder


widgetIntegrationDecoder : Decoder Integration
widgetIntegrationDecoder =
    D.map2 WidgetIntegration CommonIntegrationData.decoder WidgetIntegrationData.decoder



-- Helpers


getTypeString : Integration -> String
getTypeString integration =
    case integration of
        ApiIntegration _ ->
            "Api"

        ApiLegacyIntegration _ _ ->
            "ApiLegacy"

        WidgetIntegration _ _ ->
            "Widget"


getUuid : Integration -> String
getUuid =
    .uuid << getCommonIntegrationData


getId : Integration -> String
getId =
    .id << getCommonIntegrationData


getName : Integration -> String
getName =
    .name << getCommonIntegrationData


getVisibleName : Integration -> String
getVisibleName integration =
    let
        name =
            (getCommonIntegrationData integration).name
    in
    if String.isEmpty name then
        getId integration

    else
        name


getAnnotations : Integration -> List Annotation
getAnnotations =
    .annotations << getCommonIntegrationData


getItemUrl : Integration -> Maybe String
getItemUrl =
    .itemUrl << getCommonIntegrationData


getLogo : Integration -> Maybe String
getLogo =
    .logo << getCommonIntegrationData


getAllowCustomReply : Integration -> Maybe Bool
getAllowCustomReply =
    getApiIntegrationData (Just << .allowCustomReply)


getRequestAllowEmptySearch : Integration -> Maybe Bool
getRequestAllowEmptySearch =
    getApiLegacyIntegrationData (Just << .requestEmptySearch)


getRequestBody : Integration -> Maybe String
getRequestBody integration =
    getApiLegacyIntegrationData (Just << .requestBody) integration
        |> Maybe.orElse (getApiIntegrationData .requestBody integration)


getRequestEmptySearch : Integration -> Maybe Bool
getRequestEmptySearch =
    getApiLegacyIntegrationData (Just << .requestEmptySearch)


getRequestHeaders : Integration -> Maybe (List KeyValuePair)
getRequestHeaders integration =
    getApiLegacyIntegrationData (Just << .requestHeaders) integration
        |> Maybe.orElse (getApiIntegrationData (Just << .requestHeaders) integration)


getRequestMethod : Integration -> Maybe String
getRequestMethod integration =
    getApiLegacyIntegrationData (Just << .requestMethod) integration
        |> Maybe.orElse (getApiIntegrationData (Just << .requestMethod) integration)


getRequestUrl : Integration -> Maybe String
getRequestUrl integration =
    getApiLegacyIntegrationData (Just << .requestUrl) integration
        |> Maybe.orElse (getApiIntegrationData (Just << .requestUrl) integration)


getResponseItemId : Integration -> Maybe String
getResponseItemId =
    getApiLegacyIntegrationData .responseItemId


getResponseItemTemplate : Integration -> Maybe String
getResponseItemTemplate integration =
    getApiLegacyIntegrationData (Just << .responseItemTemplate) integration
        |> Maybe.orElse (getApiIntegrationData (Just << .responseItemTemplate) integration)


getResponseItemTemplateForSelection : Integration -> Maybe String
getResponseItemTemplateForSelection =
    getApiIntegrationData .responseItemTemplateForSelection


getResponseListField : Integration -> Maybe String
getResponseListField integration =
    getApiLegacyIntegrationData .responseListField integration
        |> Maybe.orElse (getApiIntegrationData .responseListField integration)


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
getVariables =
    .variables << getCommonIntegrationData


getWidgetUrl : Integration -> Maybe String
getWidgetUrl =
    getWidgetIntegrationData (Just << .widgetUrl)


getCommonIntegrationData : Integration -> CommonIntegrationData
getCommonIntegrationData integration =
    case integration of
        ApiIntegration data ->
            { uuid = data.uuid
            , id = ""
            , name = data.name
            , variables = data.variables
            , logo = Nothing
            , itemUrl = Nothing
            , annotations = data.annotations
            }

        ApiLegacyIntegration data _ ->
            data

        WidgetIntegration data _ ->
            data


getApiIntegrationData : (ApiIntegrationData -> Maybe a) -> Integration -> Maybe a
getApiIntegrationData map integration =
    case integration of
        ApiIntegration data ->
            map data

        _ ->
            Nothing


getApiLegacyIntegrationData : (ApiLegacyIntegrationData -> Maybe a) -> Integration -> Maybe a
getApiLegacyIntegrationData map integration =
    case integration of
        ApiLegacyIntegration _ data ->
            map data

        _ ->
            Nothing


getWidgetIntegrationData : (WidgetIntegrationData -> Maybe a) -> Integration -> Maybe a
getWidgetIntegrationData map integration =
    case integration of
        WidgetIntegration _ data ->
            map data

        _ ->
            Nothing
