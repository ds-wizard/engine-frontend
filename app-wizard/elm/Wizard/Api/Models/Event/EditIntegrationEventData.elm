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
import Wizard.Api.Models.Event.EditIntegrationPluginEventData as EditIntegrationPluginEventData exposing (EditIntegrationPluginEventData)
import Wizard.Api.Models.Event.EventField as EventField
import Wizard.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration(..))


type EditIntegrationEventData
    = EditIntegrationApiEvent EditIntegrationApiEventData
    | EditIntegrationPluginEvent EditIntegrationPluginEventData


decoder : Decoder EditIntegrationEventData
decoder =
    D.field "integrationType" D.string
        |> D.andThen
            (\integrationType ->
                case integrationType of
                    "ApiIntegration" ->
                        D.map EditIntegrationApiEvent EditIntegrationApiEventData.decoder

                    "PluginIntegration" ->
                        D.map EditIntegrationPluginEvent EditIntegrationPluginEventData.decoder

                    _ ->
                        D.fail <| "Unknown integration type: " ++ integrationType
            )


encode : EditIntegrationEventData -> List ( String, E.Value )
encode data =
    let
        eventData =
            map
                EditIntegrationApiEventData.encode
                EditIntegrationPluginEventData.encode
                data
    in
    ( "eventType", E.string "EditIntegrationEvent" ) :: eventData


apply : EditIntegrationEventData -> Integration -> Integration
apply event integration =
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

        EditIntegrationPluginEvent eventData ->
            PluginIntegration
                { annotations = EventField.getValueWithDefault eventData.annotations (Integration.getAnnotations integration)
                , name = EventField.getValueWithDefault eventData.name (Integration.getName integration)
                , pluginIntegrationId = EventField.getValueWithDefault eventData.pluginIntegrationId (Maybe.withDefault "" (Integration.getPluginIntegrationId integration))
                , pluginIntegrationSettings = EventField.getValueWithDefault eventData.pluginIntegrationSettings (Maybe.withDefault "" (Integration.getPluginIntegrationSettings integration))
                , pluginUuid = EventField.getValueWithDefault eventData.pluginUuid (Maybe.withDefault "" (Integration.getPluginUuid integration))
                , uuid = Integration.getUuid integration
                }


getTypeString : EditIntegrationEventData -> String
getTypeString =
    map
        (always "Api")
        (always "Plugin")


getEntityVisibleName : EditIntegrationEventData -> Maybe String
getEntityVisibleName =
    EventField.getValue << map .name .pluginIntegrationId


map :
    (EditIntegrationApiEventData -> a)
    -> (EditIntegrationPluginEventData -> a)
    -> EditIntegrationEventData
    -> a
map apiIntegration pluginIntegration integration =
    case integration of
        EditIntegrationApiEvent data ->
            apiIntegration data

        EditIntegrationPluginEvent data ->
            pluginIntegration data


squash : EditIntegrationEventData -> EditIntegrationEventData -> EditIntegrationEventData
squash old new =
    case ( old, new ) of
        ( EditIntegrationApiEvent oldData, EditIntegrationApiEvent newData ) ->
            EditIntegrationApiEvent (EditIntegrationApiEventData.squash oldData newData)

        ( EditIntegrationPluginEvent oldData, EditIntegrationPluginEvent newData ) ->
            EditIntegrationPluginEvent (EditIntegrationPluginEventData.squash oldData newData)

        _ ->
            new
