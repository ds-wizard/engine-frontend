module Wizard.KMEditor.Common.Events.EditQuestionEventData exposing
    ( EditQuestionEventData(..)
    , decoder
    , encode
    , getEntityVisibleName
    , getTypeString
    , map
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.KMEditor.Common.Events.EditQuestionIntegrationEventData as EditQuestionIntegrationEventData exposing (EditQuestionIntegrationEventData)
import Wizard.KMEditor.Common.Events.EditQuestionListEventData as EditQuestionListEventData exposing (EditQuestionListEventData)
import Wizard.KMEditor.Common.Events.EditQuestionOptionsEventData as EditQuestionOptionsEventData exposing (EditQuestionOptionsEventData)
import Wizard.KMEditor.Common.Events.EditQuestionValueEventData as EditQuestionValueEventData exposing (EditQuestionValueEventData)
import Wizard.KMEditor.Common.Events.EventField as EventField


type EditQuestionEventData
    = EditQuestionOptionsEvent EditQuestionOptionsEventData
    | EditQuestionListEvent EditQuestionListEventData
    | EditQuestionValueEvent EditQuestionValueEventData
    | EditQuestionIntegrationEvent EditQuestionIntegrationEventData


decoder : Decoder EditQuestionEventData
decoder =
    D.field "questionType" D.string
        |> D.andThen
            (\questionType ->
                case questionType of
                    "OptionsQuestion" ->
                        D.map EditQuestionOptionsEvent EditQuestionOptionsEventData.decoder

                    "ListQuestion" ->
                        D.map EditQuestionListEvent EditQuestionListEventData.decoder

                    "ValueQuestion" ->
                        D.map EditQuestionValueEvent EditQuestionValueEventData.decoder

                    "IntegrationQuestion" ->
                        D.map EditQuestionIntegrationEvent EditQuestionIntegrationEventData.decoder

                    _ ->
                        D.fail <| "Unknown question type: " ++ questionType
            )


encode : EditQuestionEventData -> List ( String, E.Value )
encode data =
    let
        eventData =
            map
                EditQuestionOptionsEventData.encode
                EditQuestionListEventData.encode
                EditQuestionValueEventData.encode
                EditQuestionIntegrationEventData.encode
                data
    in
    [ ( "eventType", E.string "EditQuestionEvent" ) ] ++ eventData


getTypeString : EditQuestionEventData -> String
getTypeString =
    map
        (\_ -> "Options")
        (\_ -> "List")
        (\_ -> "Value")
        (\_ -> "Integration")


getEntityVisibleName : EditQuestionEventData -> Maybe String
getEntityVisibleName =
    EventField.getValue << map .title .title .title .title


map :
    (EditQuestionOptionsEventData -> a)
    -> (EditQuestionListEventData -> a)
    -> (EditQuestionValueEventData -> a)
    -> (EditQuestionIntegrationEventData -> a)
    -> EditQuestionEventData
    -> a
map optionsQuestion listQuestion valueQuestion integrationQuestion question =
    case question of
        EditQuestionOptionsEvent data ->
            optionsQuestion data

        EditQuestionListEvent data ->
            listQuestion data

        EditQuestionValueEvent data ->
            valueQuestion data

        EditQuestionIntegrationEvent data ->
            integrationQuestion data
