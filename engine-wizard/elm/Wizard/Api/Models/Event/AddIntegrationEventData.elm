module Wizard.Api.Models.Event.AddIntegrationEventData exposing
    ( AddIntegrationEventData(..)
    , decoder
    , encode
    , getEntityVisibleName
    , getTypeString
    , init
    , map
    , toIntegration
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.Api.Models.Event.AddIntegrationApiEventData as AddIntegrationApiEventData exposing (AddIntegrationApiEventData)
import Wizard.Api.Models.Event.AddIntegrationApiLegacyEventData as AddIntegrationApiLegacyEventData exposing (AddIntegrationApiLegacyEventData)
import Wizard.Api.Models.Event.AddIntegrationWidgetEventData as AddIntegrationWidgetEventData exposing (AddIntegrationWidgetEventData)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration)


type AddIntegrationEventData
    = AddIntegrationApiEvent AddIntegrationApiEventData
    | AddIntegrationApiLegacyEvent AddIntegrationApiLegacyEventData
    | AddIntegrationWidgetEvent AddIntegrationWidgetEventData


decoder : Decoder AddIntegrationEventData
decoder =
    D.field "integrationType" D.string
        |> D.andThen
            (\integrationType ->
                case integrationType of
                    "ApiIntegration" ->
                        D.map AddIntegrationApiEvent AddIntegrationApiEventData.decoder

                    "ApiLegacyIntegration" ->
                        D.map AddIntegrationApiLegacyEvent AddIntegrationApiLegacyEventData.decoder

                    "WidgetIntegration" ->
                        D.map AddIntegrationWidgetEvent AddIntegrationWidgetEventData.decoder

                    _ ->
                        D.fail <| "Unknown integration type: " ++ integrationType
            )


encode : AddIntegrationEventData -> List ( String, E.Value )
encode data =
    let
        eventData =
            map
                AddIntegrationApiEventData.encode
                AddIntegrationApiLegacyEventData.encode
                AddIntegrationWidgetEventData.encode
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

        AddIntegrationApiLegacyEvent eventData ->
            AddIntegrationApiLegacyEventData.toIntegration uuid eventData

        AddIntegrationWidgetEvent eventData ->
            AddIntegrationWidgetEventData.toIntegration uuid eventData


getTypeString : AddIntegrationEventData -> String
getTypeString =
    map
        (\_ -> "Api")
        (\_ -> "ApiLegacy")
        (\_ -> "Widget")


getEntityVisibleName : AddIntegrationEventData -> Maybe String
getEntityVisibleName =
    Just << map .name .name .name


map :
    (AddIntegrationApiEventData -> a)
    -> (AddIntegrationApiLegacyEventData -> a)
    -> (AddIntegrationWidgetEventData -> a)
    -> AddIntegrationEventData
    -> a
map apiIntegration apiLegacyIntegration widgetIntegration integration =
    case integration of
        AddIntegrationApiEvent data ->
            apiIntegration data

        AddIntegrationApiLegacyEvent data ->
            apiLegacyIntegration data

        AddIntegrationWidgetEvent data ->
            widgetIntegration data
