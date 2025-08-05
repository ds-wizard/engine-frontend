module Wizard.Api.Models.KnowledgeModel.Integration exposing
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
    , getVisibleName
    , getWidgetUrl
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Wizard.Api.Models.KnowledgeModel.Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Integration.ApiIntegrationData as ApiIntegrationData exposing (ApiIntegrationData)
import Wizard.Api.Models.KnowledgeModel.Integration.CommonIntegrationData as CommonIntegrationData exposing (CommonIntegrationData)
import Wizard.Api.Models.KnowledgeModel.Integration.IntegrationType as IntegrationType
import Wizard.Api.Models.KnowledgeModel.Integration.RequestHeader exposing (RequestHeader)
import Wizard.Api.Models.KnowledgeModel.Integration.WidgetIntegrationData as WidgetIntegrationData exposing (WidgetIntegrationData)


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


getProps : Integration -> List String
getProps =
    .props << getCommonIntegrationData


getLogo : Integration -> Maybe String
getLogo =
    .logo << getCommonIntegrationData


getItemUrl : Integration -> Maybe String
getItemUrl =
    .itemUrl << getCommonIntegrationData


getAnnotations : Integration -> List Annotation
getAnnotations =
    .annotations << getCommonIntegrationData


getRequestMethod : Integration -> Maybe String
getRequestMethod =
    getApiIntegrationData (Just << .requestMethod)


getRequestUrl : Integration -> Maybe String
getRequestUrl =
    getApiIntegrationData (Just << .requestUrl)


getRequestHeaders : Integration -> Maybe (List RequestHeader)
getRequestHeaders =
    getApiIntegrationData (Just << .requestHeaders)


getRequestBody : Integration -> Maybe String
getRequestBody =
    getApiIntegrationData (Just << .requestBody)


getRequestEmptySearch : Integration -> Maybe Bool
getRequestEmptySearch =
    getApiIntegrationData (Just << .requestEmptySearch)


getResponseListField : Integration -> Maybe String
getResponseListField =
    getApiIntegrationData .responseListField


getResponseItemId : Integration -> Maybe String
getResponseItemId =
    getApiIntegrationData .responseItemId


getResponseItemTemplate : Integration -> Maybe String
getResponseItemTemplate =
    getApiIntegrationData (Just << .responseItemTemplate)


getWidgetUrl : Integration -> Maybe String
getWidgetUrl =
    getWidgetIntegrationData (Just << .widgetUrl)


getCommonIntegrationData : Integration -> CommonIntegrationData
getCommonIntegrationData integration =
    case integration of
        ApiIntegration data _ ->
            data

        WidgetIntegration data _ ->
            data


getApiIntegrationData : (ApiIntegrationData -> Maybe a) -> Integration -> Maybe a
getApiIntegrationData map integration =
    case integration of
        ApiIntegration _ data ->
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
