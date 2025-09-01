module Wizard.Api.Models.Event.AddQuestionEventData exposing
    ( AddQuestionEventData(..)
    , decoder
    , encode
    , getEntityVisibleName
    , getTypeString
    , init
    , map
    , toQuestion
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.Api.Models.Event.AddQuestionFileEventData as AddQuestionFileEventData exposing (AddQuestionFileEventData)
import Wizard.Api.Models.Event.AddQuestionIntegrationEventData as AddQuestionIntegrationEventData exposing (AddQuestionIntegrationEventData)
import Wizard.Api.Models.Event.AddQuestionItemSelectEventData as AddQuestionItemSelectEventData exposing (AddQuestionItemSelectEventData)
import Wizard.Api.Models.Event.AddQuestionListEventData as AddQuestionListEventData exposing (AddQuestionListEventData)
import Wizard.Api.Models.Event.AddQuestionMultiChoiceEventData as AddQuestionMultiChoiceEventData exposing (AddQuestionMultiChoiceEventData)
import Wizard.Api.Models.Event.AddQuestionOptionsEventData as AddQuestionOptionsEventData exposing (AddQuestionOptionsEventData)
import Wizard.Api.Models.Event.AddQuestionValueEventData as AddQuestionValueEventData exposing (AddQuestionValueEventData)
import Wizard.Api.Models.KnowledgeModel.Question exposing (Question)


type AddQuestionEventData
    = AddQuestionOptionsEvent AddQuestionOptionsEventData
    | AddQuestionListEvent AddQuestionListEventData
    | AddQuestionValueEvent AddQuestionValueEventData
    | AddQuestionIntegrationEvent AddQuestionIntegrationEventData
    | AddQuestionMultiChoiceEvent AddQuestionMultiChoiceEventData
    | AddQuestionItemSelectEvent AddQuestionItemSelectEventData
    | AddQuestionFileEvent AddQuestionFileEventData


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

                    "MultiChoiceQuestion" ->
                        D.map AddQuestionMultiChoiceEvent AddQuestionMultiChoiceEventData.decoder

                    "ItemSelectQuestion" ->
                        D.map AddQuestionItemSelectEvent AddQuestionItemSelectEventData.decoder

                    "FileQuestion" ->
                        D.map AddQuestionFileEvent AddQuestionFileEventData.decoder

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
                AddQuestionMultiChoiceEventData.encode
                AddQuestionItemSelectEventData.encode
                AddQuestionFileEventData.encode
                data
    in
    ( "eventType", E.string "AddQuestionEvent" ) :: eventData


init : AddQuestionEventData
init =
    AddQuestionOptionsEvent AddQuestionOptionsEventData.init


toQuestion : String -> AddQuestionEventData -> Question
toQuestion questionUuid data =
    case data of
        AddQuestionOptionsEvent eventData ->
            AddQuestionOptionsEventData.toQuestion questionUuid eventData

        AddQuestionListEvent eventData ->
            AddQuestionListEventData.toQuestion questionUuid eventData

        AddQuestionValueEvent eventData ->
            AddQuestionValueEventData.toQuestion questionUuid eventData

        AddQuestionIntegrationEvent eventData ->
            AddQuestionIntegrationEventData.toQuestion questionUuid eventData

        AddQuestionMultiChoiceEvent eventData ->
            AddQuestionMultiChoiceEventData.toQuestion questionUuid eventData

        AddQuestionItemSelectEvent eventData ->
            AddQuestionItemSelectEventData.toQuestion questionUuid eventData

        AddQuestionFileEvent eventData ->
            AddQuestionFileEventData.toQuestion questionUuid eventData


getTypeString : AddQuestionEventData -> String
getTypeString =
    map
        (always "Options")
        (always "List")
        (always "Value")
        (always "Integration")
        (always "MultiChoice")
        (always "ItemSelect")
        (always "File")


getEntityVisibleName : AddQuestionEventData -> Maybe String
getEntityVisibleName =
    Just << map .title .title .title .title .title .title .title


map :
    (AddQuestionOptionsEventData -> a)
    -> (AddQuestionListEventData -> a)
    -> (AddQuestionValueEventData -> a)
    -> (AddQuestionIntegrationEventData -> a)
    -> (AddQuestionMultiChoiceEventData -> a)
    -> (AddQuestionItemSelectEventData -> a)
    -> (AddQuestionFileEventData -> a)
    -> AddQuestionEventData
    -> a
map optionsQuestion listQuestion valueQuestion integrationQuestion multiChoiceQuestion itemSelectQuestion fileQuestion question =
    case question of
        AddQuestionOptionsEvent data ->
            optionsQuestion data

        AddQuestionListEvent data ->
            listQuestion data

        AddQuestionValueEvent data ->
            valueQuestion data

        AddQuestionIntegrationEvent data ->
            integrationQuestion data

        AddQuestionMultiChoiceEvent data ->
            multiChoiceQuestion data

        AddQuestionItemSelectEvent data ->
            itemSelectQuestion data

        AddQuestionFileEvent data ->
            fileQuestion data
