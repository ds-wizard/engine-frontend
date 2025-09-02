module Wizard.Api.Models.Event.EditIntegrationEventData exposing
    ( EditIntegrationEventData(..)
    , apply
    , decoder
    , encode
    , getEntityVisibleName
    , getTypeString
    , map
    , squash
    )

import Dict
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.Api.Models.Event.EditIntegrationApiEventData as EditIntegrationApiEventData exposing (EditIntegrationApiEventData)
import Wizard.Api.Models.Event.EditIntegrationApiLegacyEventData as EditIntegrationApiLegacyEventData exposing (EditIntegrationApiLegacyEventData)
import Wizard.Api.Models.Event.EditIntegrationWidgetEventData as EditIntegrationWidgetEventData exposing (EditIntegrationWidgetEventData)
import Wizard.Api.Models.Event.EventField as EventField
import Wizard.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration(..))


type EditIntegrationEventData
    = EditIntegrationApiEvent EditIntegrationApiEventData
    | EditIntegrationApiLegacyEvent EditIntegrationApiLegacyEventData
    | EditIntegrationWidgetEvent EditIntegrationWidgetEventData


decoder : Decoder EditIntegrationEventData
decoder =
    D.field "integrationType" D.string
        |> D.andThen
            (\integrationType ->
                case integrationType of
                    "ApiIntegration" ->
                        D.map EditIntegrationApiEvent EditIntegrationApiEventData.decoder

                    "ApiLegacyIntegration" ->
                        D.map EditIntegrationApiLegacyEvent EditIntegrationApiLegacyEventData.decoder

                    "WidgetIntegration" ->
                        D.map EditIntegrationWidgetEvent EditIntegrationWidgetEventData.decoder

                    _ ->
                        D.fail <| "Unknown integration type: " ++ integrationType
            )


encode : EditIntegrationEventData -> List ( String, E.Value )
encode data =
    let
        eventData =
            map
                EditIntegrationApiEventData.encode
                EditIntegrationApiLegacyEventData.encode
                EditIntegrationWidgetEventData.encode
                data
    in
    ( "eventType", E.string "EditIntegrationEvent" ) :: eventData


apply : EditIntegrationEventData -> Integration -> Integration
apply event integration =
    let
        applyCommonData data =
            { uuid = Integration.getUuid integration
            , id = EventField.getValueWithDefault data.id (Integration.getId integration)
            , name = EventField.getValueWithDefault data.name (Integration.getName integration)
            , variables = EventField.getValueWithDefault data.variables (Integration.getVariables integration)
            , logo = EventField.getValueWithDefault data.logo (Integration.getLogo integration)
            , itemUrl = EventField.getValueWithDefault data.itemUrl (Integration.getItemUrl integration)
            , annotations = EventField.getValueWithDefault data.annotations (Integration.getAnnotations integration)
            }
    in
    case event of
        EditIntegrationApiEvent eventData ->
            ApiIntegration
                { allowCustomReply = EventField.getValueWithDefault eventData.allowCustomReply (Maybe.withDefault True (Integration.getAllowCustomReply integration))
                , annotations = EventField.getValueWithDefault eventData.annotations (Integration.getAnnotations integration)
                , name = EventField.getValueWithDefault eventData.name (Integration.getName integration)
                , requestAllowEmptySearch = EventField.getValueWithDefault eventData.requestAllowEmptySearch (Maybe.withDefault False (Integration.getRequestAllowEmptySearch integration))
                , requestBody = EventField.getValueWithDefault eventData.requestBody (Integration.getRequestBody integration)
                , requestHeaders = EventField.getValueWithDefault eventData.requestHeaders (Maybe.withDefault [] (Integration.getRequestHeaders integration))
                , requestMethod = EventField.getValueWithDefault eventData.requestMethod (Maybe.withDefault "" (Integration.getRequestMethod integration))
                , requestUrl = EventField.getValueWithDefault eventData.requestUrl (Maybe.withDefault "" (Integration.getRequestUrl integration))
                , responseItemTemplate = EventField.getValueWithDefault eventData.responseItemTemplate (Maybe.withDefault "" (Integration.getResponseItemTemplate integration))
                , responseItemTemplateForSelection = EventField.getValueWithDefault eventData.responseItemTemplateForSelection (Integration.getResponseItemTemplateForSelection integration)
                , responseListField = EventField.getValueWithDefault eventData.responseListField (Integration.getResponseListField integration)
                , testQ = EventField.getValueWithDefault eventData.testQ (Maybe.withDefault "" (Integration.getTestQ integration))
                , testResponse = EventField.getValueWithDefault eventData.testResponse (Integration.getTestResponse integration)
                , testVariables = EventField.getValueWithDefault eventData.testVariables (Maybe.withDefault Dict.empty (Integration.getTestVariables integration))
                , uuid = Integration.getUuid integration
                , variables = EventField.getValueWithDefault eventData.variables (Integration.getVariables integration)
                }

        EditIntegrationApiLegacyEvent eventData ->
            ApiLegacyIntegration
                (applyCommonData eventData)
                { requestMethod = EventField.getValueWithDefault eventData.requestMethod (Maybe.withDefault "" (Integration.getRequestMethod integration))
                , requestUrl = EventField.getValueWithDefault eventData.requestUrl (Maybe.withDefault "" (Integration.getRequestUrl integration))
                , requestHeaders = EventField.getValueWithDefault eventData.requestHeaders (Maybe.withDefault [] (Integration.getRequestHeaders integration))
                , requestBody = EventField.getValueWithDefault eventData.requestBody (Maybe.withDefault "" (Integration.getRequestBody integration))
                , requestEmptySearch = EventField.getValueWithDefault eventData.requestEmptySearch (Maybe.withDefault True (Integration.getRequestEmptySearch integration))
                , responseListField = EventField.getValueWithDefault eventData.responseListField (Integration.getResponseListField integration)
                , responseItemId = EventField.getValueWithDefault eventData.responseItemId (Integration.getResponseItemId integration)
                , responseItemTemplate = EventField.getValueWithDefault eventData.responseItemTemplate (Maybe.withDefault "" (Integration.getResponseItemTemplate integration))
                }

        EditIntegrationWidgetEvent eventData ->
            WidgetIntegration
                (applyCommonData eventData)
                { widgetUrl = EventField.getValueWithDefault eventData.widgetUrl (Maybe.withDefault "" (Integration.getWidgetUrl integration))
                }


getTypeString : EditIntegrationEventData -> String
getTypeString =
    map
        (\_ -> "Api")
        (\_ -> "ApiLegacy")
        (\_ -> "Widget")


getEntityVisibleName : EditIntegrationEventData -> Maybe String
getEntityVisibleName =
    EventField.getValue << map .name .name .name


map :
    (EditIntegrationApiEventData -> a)
    -> (EditIntegrationApiLegacyEventData -> a)
    -> (EditIntegrationWidgetEventData -> a)
    -> EditIntegrationEventData
    -> a
map apiIntegration apiLegacyIntegration widgetIntegration integration =
    case integration of
        EditIntegrationApiEvent data ->
            apiIntegration data

        EditIntegrationApiLegacyEvent data ->
            apiLegacyIntegration data

        EditIntegrationWidgetEvent data ->
            widgetIntegration data


squash : EditIntegrationEventData -> EditIntegrationEventData -> EditIntegrationEventData
squash old new =
    case ( old, new ) of
        ( EditIntegrationApiEvent oldData, EditIntegrationApiEvent newData ) ->
            EditIntegrationApiEvent (EditIntegrationApiEventData.squash oldData newData)

        ( EditIntegrationApiLegacyEvent oldData, EditIntegrationApiLegacyEvent newData ) ->
            EditIntegrationApiLegacyEvent (EditIntegrationApiLegacyEventData.squash oldData newData)

        ( EditIntegrationWidgetEvent oldData, EditIntegrationWidgetEvent newData ) ->
            EditIntegrationWidgetEvent (EditIntegrationWidgetEventData.squash oldData newData)

        _ ->
            new
