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
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration)


type AddIntegrationEventData
    = AddIntegrationApiEvent AddIntegrationApiEventData


decoder : Decoder AddIntegrationEventData
decoder =
    D.field "integrationType" D.string
        |> D.andThen
            (\integrationType ->
                case integrationType of
                    "ApiIntegration" ->
                        D.map AddIntegrationApiEvent AddIntegrationApiEventData.decoder

                    _ ->
                        D.fail <| "Unknown integration type: " ++ integrationType
            )


encode : AddIntegrationEventData -> List ( String, E.Value )
encode data =
    let
        eventData =
            map
                AddIntegrationApiEventData.encode
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


getTypeString : AddIntegrationEventData -> String
getTypeString =
    map
        (\_ -> "Api")


getEntityVisibleName : AddIntegrationEventData -> Maybe String
getEntityVisibleName =
    Just << map .name


map :
    (AddIntegrationApiEventData -> a)
    -> AddIntegrationEventData
    -> a
map apiIntegration integration =
    case integration of
        AddIntegrationApiEvent data ->
            apiIntegration data
