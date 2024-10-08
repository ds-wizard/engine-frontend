module Shared.Data.Event.EditIntegrationEventData exposing
    ( EditIntegrationEventData(..)
    , apply
    , decoder
    , encode
    , getEntityVisibleName
    , getTypeString
    , map
    , squash
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Shared.Data.Event.EditIntegrationApiEventData as EditIntegrationApiEventData exposing (EditIntegrationApiEventData)
import Shared.Data.Event.EditIntegrationWidgetEventData as EditIntegrationWidgetEventData exposing (EditIntegrationWidgetEventData)
import Shared.Data.Event.EventField as EventField
import Shared.Data.KnowledgeModel.Integration as Integration exposing (Integration(..))


type EditIntegrationEventData
    = EditIntegrationApiEvent EditIntegrationApiEventData
    | EditIntegrationWidgetEvent EditIntegrationWidgetEventData


decoder : Decoder EditIntegrationEventData
decoder =
    D.field "integrationType" D.string
        |> D.andThen
            (\integrationType ->
                case integrationType of
                    "ApiIntegration" ->
                        D.map EditIntegrationApiEvent EditIntegrationApiEventData.decoder

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
            , props = EventField.getValueWithDefault data.props (Integration.getProps integration)
            , logo = EventField.getValueWithDefault data.logo (Integration.getLogo integration)
            , itemUrl = EventField.getValueWithDefault data.itemUrl (Integration.getItemUrl integration)
            , annotations = EventField.getValueWithDefault data.annotations (Integration.getAnnotations integration)
            }
    in
    case event of
        EditIntegrationApiEvent eventData ->
            ApiIntegration
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
        (\_ -> "Widget")


getEntityVisibleName : EditIntegrationEventData -> Maybe String
getEntityVisibleName =
    EventField.getValue << map .name .name


map :
    (EditIntegrationApiEventData -> a)
    -> (EditIntegrationWidgetEventData -> a)
    -> EditIntegrationEventData
    -> a
map apiIntegration widgetIntegration integration =
    case integration of
        EditIntegrationApiEvent data ->
            apiIntegration data

        EditIntegrationWidgetEvent data ->
            widgetIntegration data


squash : EditIntegrationEventData -> EditIntegrationEventData -> EditIntegrationEventData
squash old new =
    case ( old, new ) of
        ( EditIntegrationApiEvent oldData, EditIntegrationApiEvent newData ) ->
            EditIntegrationApiEvent (EditIntegrationApiEventData.squash oldData newData)

        ( EditIntegrationWidgetEvent oldData, EditIntegrationWidgetEvent newData ) ->
            EditIntegrationWidgetEvent (EditIntegrationWidgetEventData.squash oldData newData)

        _ ->
            new
