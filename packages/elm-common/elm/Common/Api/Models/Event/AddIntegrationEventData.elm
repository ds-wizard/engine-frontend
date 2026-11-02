module Common.Api.Models.Event.AddIntegrationEventData exposing
    ( AddIntegrationEventData(..)
    , decoder
    , encode
    , getEntityVisibleName
    , getTypeString
    , init
    , map
    , toIntegration
    )

import Common.Api.Models.Event.AddIntegrationApiEventData as AddIntegrationApiEventData exposing (AddIntegrationApiEventData)
import Common.Api.Models.Event.AddIntegrationPluginEventData as AddIntegrationPluginEventData exposing (AddIntegrationPluginEventData)
import Common.Api.Models.KnowledgeModel.Integration exposing (Integration)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type AddIntegrationEventData
    = AddIntegrationApiEvent AddIntegrationApiEventData
    | AddIntegrationPluginEvent AddIntegrationPluginEventData


decoder : Decoder AddIntegrationEventData
decoder =
    D.field "integrationType" D.string
        |> D.andThen
            (\integrationType ->
                case integrationType of
                    "ApiIntegration" ->
                        D.map AddIntegrationApiEvent AddIntegrationApiEventData.decoder

                    "PluginIntegration" ->
                        D.map AddIntegrationPluginEvent AddIntegrationPluginEventData.decoder

                    _ ->
                        D.fail <| "Unknown integration type: " ++ integrationType
            )


encode : AddIntegrationEventData -> List ( String, E.Value )
encode data =
    let
        eventData =
            map
                AddIntegrationApiEventData.encode
                AddIntegrationPluginEventData.encode
                data
    in
    ( "eventType", E.string "AddIntegrationEvent" ) :: eventData


init : AddIntegrationEventData
init =
    AddIntegrationApiEvent AddIntegrationApiEventData.init


toIntegration : String -> AddIntegrationEventData -> Integration
toIntegration uuid data =
    case data of
        AddIntegrationApiEvent eventData ->
            AddIntegrationApiEventData.toIntegration uuid eventData

        AddIntegrationPluginEvent eventData ->
            AddIntegrationPluginEventData.toIntegration uuid eventData


getTypeString : AddIntegrationEventData -> String
getTypeString =
    map
        (always "Api")
        (always "Plugin")


getEntityVisibleName : AddIntegrationEventData -> Maybe String
getEntityVisibleName =
    Just << map .name .pluginIntegrationId


map :
    (AddIntegrationApiEventData -> a)
    -> (AddIntegrationPluginEventData -> a)
    -> AddIntegrationEventData
    -> a
map apiIntegration pluginIntegration integration =
    case integration of
        AddIntegrationApiEvent data ->
            apiIntegration data

        AddIntegrationPluginEvent data ->
            pluginIntegration data
