module Shared.Data.KnowledgeModel.Integration exposing
    ( Integration(..)
    , decoder
    , getAnnotations
    , getId
    , getItemUrl
    , getLogo
    , getName
    , getProps
    , getRequestBody
    , getRequestEmptySearch
    , getRequestHeaders
    , getRequestMethod
    , getRequestUrl
    , getResponseItemId
    , getResponseItemTemplate
    , getResponseListField
    , getTypeString
    , getUuid
    , getWidgetUrl
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Shared.Data.KnowledgeModel.Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Integration.ApiIntegrationData as ApiIntegrationData exposing (ApiIntegrationData)
import Shared.Data.KnowledgeModel.Integration.CommonIntegrationData as CommonIntegrationData exposing (CommonIntegrationData)
import Shared.Data.KnowledgeModel.Integration.IntegrationType as IntegrationType
import Shared.Data.KnowledgeModel.Integration.RequestHeader exposing (RequestHeader)
import Shared.Data.KnowledgeModel.Integration.WidgetIntegrationData as WidgetIntegrationData exposing (WidgetIntegrationData)


type Integration
    = ApiIntegration CommonIntegrationData ApiIntegrationData
    | WidgetIntegration CommonIntegrationData WidgetIntegrationData



-- Decoders


decoder : Decoder Integration
decoder =
    D.oneOf
        [ D.when IntegrationType.decoder ((==) IntegrationType.Api) apiIntegrationDecoder
        , D.when IntegrationType.decoder ((==) IntegrationType.Widget) widgetIntegrationDecoder
        ]


apiIntegrationDecoder : Decoder Integration
apiIntegrationDecoder =
    D.map2 ApiIntegration CommonIntegrationData.decoder ApiIntegrationData.decoder


widgetIntegrationDecoder : Decoder Integration
widgetIntegrationDecoder =
    D.map2 WidgetIntegration CommonIntegrationData.decoder WidgetIntegrationData.decoder



-- Helpers


getTypeString : Integration -> String
getTypeString integration =
    case integration of
        ApiIntegration _ _ ->
            "Api"

        WidgetIntegration _ _ ->
            "Widget"


getUuid : Integration -> String
getUuid =
    .uuid << getCommonIntegrationData


getId : Integration -> String
getId =
    .id << getCommonIntegrationData


getName : Integration -> String
getName integration =
    let
        name =
            (getCommonIntegrationData integration).name
    in
    if String.isEmpty name then
        getId integration

    else
        name


getProps : Integration -> List String
getProps =
    .props << getCommonIntegrationData


getLogo : Integration -> String
getLogo =
    .logo << getCommonIntegrationData


getItemUrl : Integration -> String
getItemUrl =
    .itemUrl << getCommonIntegrationData


getAnnotations : Integration -> List Annotation
getAnnotations =
    .annotations << getCommonIntegrationData


getRequestMethod : Integration -> Maybe String
getRequestMethod =
    getApiIntegrationData .requestMethod


getRequestUrl : Integration -> Maybe String
getRequestUrl =
    getApiIntegrationData .requestUrl


getRequestHeaders : Integration -> Maybe (List RequestHeader)
getRequestHeaders =
    getApiIntegrationData .requestHeaders


getRequestBody : Integration -> Maybe String
getRequestBody =
    getApiIntegrationData .requestBody


getRequestEmptySearch : Integration -> Maybe Bool
getRequestEmptySearch =
    getApiIntegrationData .requestEmptySearch


getResponseListField : Integration -> Maybe String
getResponseListField =
    getApiIntegrationData .responseListField


getResponseItemId : Integration -> Maybe String
getResponseItemId =
    getApiIntegrationData .responseItemId


getResponseItemTemplate : Integration -> Maybe String
getResponseItemTemplate =
    getApiIntegrationData .responseItemTemplate


getWidgetUrl : Integration -> Maybe String
getWidgetUrl =
    getWidgetIntegrationData .widgetUrl


getCommonIntegrationData : Integration -> CommonIntegrationData
getCommonIntegrationData integration =
    case integration of
        ApiIntegration data _ ->
            data

        WidgetIntegration data _ ->
            data


getApiIntegrationData : (ApiIntegrationData -> a) -> Integration -> Maybe a
getApiIntegrationData map integration =
    case integration of
        ApiIntegration _ data ->
            Just <| map data

        _ ->
            Nothing


getWidgetIntegrationData : (WidgetIntegrationData -> a) -> Integration -> Maybe a
getWidgetIntegrationData map integration =
    case integration of
        WidgetIntegration _ data ->
            Just <| map data

        _ ->
            Nothing
