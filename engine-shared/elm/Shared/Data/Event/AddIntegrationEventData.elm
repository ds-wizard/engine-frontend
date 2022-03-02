module Shared.Data.Event.AddIntegrationEventData exposing
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
import Shared.Data.Event.AddIntegrationApiEventData as AddIntegrationApiEventData exposing (AddIntegrationApiEventData)
import Shared.Data.Event.AddIntegrationWidgetEventData as AddIntegrationWidgetEventData exposing (AddIntegrationWidgetEventData)
import Shared.Data.KnowledgeModel.Integration exposing (Integration)


type AddIntegrationEventData
    = AddIntegrationApiEvent AddIntegrationApiEventData
    | AddIntegrationWidgetEvent AddIntegrationWidgetEventData


decoder : Decoder AddIntegrationEventData
decoder =
    D.field "integrationType" D.string
        |> D.andThen
            (\integrationType ->
                case integrationType of
                    "ApiIntegration" ->
                        D.map AddIntegrationApiEvent AddIntegrationApiEventData.decoder

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

        AddIntegrationWidgetEvent eventData ->
            AddIntegrationWidgetEventData.toIntegration uuid eventData


getTypeString : AddIntegrationEventData -> String
getTypeString =
    map
        (\_ -> "Api")
        (\_ -> "Widget")


getEntityVisibleName : AddIntegrationEventData -> Maybe String
getEntityVisibleName =
    Just << map .name .name


map :
    (AddIntegrationApiEventData -> a)
    -> (AddIntegrationWidgetEventData -> a)
    -> AddIntegrationEventData
    -> a
map apiIntegration widgetIntegration integration =
    case integration of
        AddIntegrationApiEvent data ->
            apiIntegration data

        AddIntegrationWidgetEvent data ->
            widgetIntegration data
