module KMEditor.Common.Events.AddQuestionEventData exposing
    ( AddQuestionEventData(..)
    , decoder
    , encode
    , getEntityVisibleName
    , getTypeString
    , map
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import KMEditor.Common.Events.AddQuestionIntegrationEventData as AddQuestionIntegrationEventData exposing (AddQuestionIntegrationEventData)
import KMEditor.Common.Events.AddQuestionListEventData as AddQuestionListEventData exposing (AddQuestionListEventData)
import KMEditor.Common.Events.AddQuestionOptionsEventData as AddQuestionOptionsEventData exposing (AddQuestionOptionsEventData)
import KMEditor.Common.Events.AddQuestionValueEventData as AddQuestionValueEventData exposing (AddQuestionValueEventData)


type AddQuestionEventData
    = AddQuestionOptionsEvent AddQuestionOptionsEventData
    | AddQuestionListEvent AddQuestionListEventData
    | AddQuestionValueEvent AddQuestionValueEventData
    | AddQuestionIntegrationEvent AddQuestionIntegrationEventData


decoder : Decoder AddQuestionEventData
decoder =
    D.field "questionType" D.string
        |> D.andThen
            (\questionType ->
                case questionType of
                    "OptionsQuestion" ->
                        D.map AddQuestionOptionsEvent AddQuestionOptionsEventData.decoder

                    "ListQuestion" ->
                        D.map AddQuestionListEvent AddQuestionListEventData.decoder

                    "ValueQuestion" ->
                        D.map AddQuestionValueEvent AddQuestionValueEventData.decoder

                    "IntegrationQuestion" ->
                        D.map AddQuestionIntegrationEvent AddQuestionIntegrationEventData.decoder

                    _ ->
                        D.fail <| "Unknown question type: " ++ questionType
            )


encode : AddQuestionEventData -> List ( String, E.Value )
encode data =
    let
        eventData =
            map
                AddQuestionOptionsEventData.encode
                AddQuestionListEventData.encode
                AddQuestionValueEventData.encode
                AddQuestionIntegrationEventData.encode
                data
    in
    [ ( "eventType", E.string "AddQuestionEvent" ) ] ++ eventData


getTypeString : AddQuestionEventData -> String
getTypeString =
    map
        (\_ -> "Options")
        (\_ -> "List")
        (\_ -> "Value")
        (\_ -> "Integration")


getEntityVisibleName : AddQuestionEventData -> Maybe String
getEntityVisibleName =
    Just << map .title .title .title .title


map :
    (AddQuestionOptionsEventData -> a)
    -> (AddQuestionListEventData -> a)
    -> (AddQuestionValueEventData -> a)
    -> (AddQuestionIntegrationEventData -> a)
    -> AddQuestionEventData
    -> a
map optionsQuestion listQuestion valueQuestion integrationQuestion question =
    case question of
        AddQuestionOptionsEvent data ->
            optionsQuestion data

        AddQuestionListEvent data ->
            listQuestion data

        AddQuestionValueEvent data ->
            valueQuestion data

        AddQuestionIntegrationEvent data ->
            integrationQuestion data
