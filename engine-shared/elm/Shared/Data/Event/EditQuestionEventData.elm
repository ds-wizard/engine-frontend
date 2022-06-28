module Shared.Data.Event.EditQuestionEventData exposing
    ( EditQuestionEventData(..)
    , apply
    , decoder
    , encode
    , getEntityVisibleName
    , getTypeString
    , map
    )

import Dict
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Shared.Data.Event.EditQuestionIntegrationEventData as EditQuestionIntegrationEventData exposing (EditQuestionIntegrationEventData)
import Shared.Data.Event.EditQuestionListEventData as EditQuestionListEventData exposing (EditQuestionListEventData)
import Shared.Data.Event.EditQuestionMultiChoiceEventData as EditQuestionMultiChoiceEventData exposing (EditQuestionMultiChoiceEventData)
import Shared.Data.Event.EditQuestionOptionsEventData as EditQuestionOptionsEventData exposing (EditQuestionOptionsEventData)
import Shared.Data.Event.EditQuestionValueEventData as EditQuestionValueEventData exposing (EditQuestionValueEventData)
import Shared.Data.Event.EventField as EventField
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Question.QuestionValueType as QuestionValueType


type EditQuestionEventData
    = EditQuestionOptionsEvent EditQuestionOptionsEventData
    | EditQuestionListEvent EditQuestionListEventData
    | EditQuestionValueEvent EditQuestionValueEventData
    | EditQuestionIntegrationEvent EditQuestionIntegrationEventData
    | EditQuestionMultiChoiceEvent EditQuestionMultiChoiceEventData


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

                    "MultiChoiceQuestion" ->
                        D.map EditQuestionMultiChoiceEvent EditQuestionMultiChoiceEventData.decoder

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
                EditQuestionMultiChoiceEventData.encode
                data
    in
    ( "eventType", E.string "EditQuestionEvent" ) :: eventData


apply : EditQuestionEventData -> Question -> Question
apply event question =
    let
        applyCommonData data =
            { uuid = Question.getUuid question
            , title = EventField.getValueWithDefault data.title (Question.getTitle question)
            , text = EventField.getValueWithDefault data.text (Question.getText question)
            , requiredPhaseUuid = EventField.getValueWithDefault data.requiredPhaseUuid (Question.getRequiredPhaseUuid question)
            , tagUuids = EventField.getValueWithDefault data.tagUuids (Question.getTagUuids question)
            , referenceUuids = EventField.applyChildren data.referenceUuids (Question.getReferenceUuids question)
            , expertUuids = EventField.applyChildren data.expertUuids (Question.getExpertUuids question)
            , annotations = EventField.getValueWithDefault data.annotations (Question.getAnnotations question)
            }
    in
    case event of
        EditQuestionOptionsEvent eventData ->
            OptionsQuestion
                (applyCommonData eventData)
                { answerUuids = EventField.applyChildren eventData.answerUuids (Question.getAnswerUuids question)
                }

        EditQuestionListEvent eventData ->
            ListQuestion
                (applyCommonData eventData)
                { itemTemplateQuestionUuids = EventField.applyChildren eventData.itemTemplateQuestionUuids (Question.getItemTemplateQuestionUuids question)
                }

        EditQuestionValueEvent eventData ->
            ValueQuestion
                (applyCommonData eventData)
                { valueType = EventField.getValueWithDefault eventData.valueType (Maybe.withDefault QuestionValueType.default (Question.getValueType question))
                }

        EditQuestionIntegrationEvent eventData ->
            IntegrationQuestion
                (applyCommonData eventData)
                { integrationUuid = EventField.getValueWithDefault eventData.integrationUuid (Maybe.withDefault "" (Question.getIntegrationUuid question))
                , props = EventField.getValueWithDefault eventData.props (Maybe.withDefault Dict.empty (Question.getProps question))
                }

        EditQuestionMultiChoiceEvent eventData ->
            MultiChoiceQuestion
                (applyCommonData eventData)
                { choiceUuids = EventField.applyChildren eventData.choiceUuids (Question.getChoiceUuids question)
                }


getTypeString : EditQuestionEventData -> String
getTypeString =
    map
        (\_ -> "Options")
        (\_ -> "List")
        (\_ -> "Value")
        (\_ -> "Integration")
        (\_ -> "MultiChoice")


getEntityVisibleName : EditQuestionEventData -> Maybe String
getEntityVisibleName =
    EventField.getValue << map .title .title .title .title .title


map :
    (EditQuestionOptionsEventData -> a)
    -> (EditQuestionListEventData -> a)
    -> (EditQuestionValueEventData -> a)
    -> (EditQuestionIntegrationEventData -> a)
    -> (EditQuestionMultiChoiceEventData -> a)
    -> EditQuestionEventData
    -> a
map optionsQuestion listQuestion valueQuestion integrationQuestion multiChoiceQuestion question =
    case question of
        EditQuestionOptionsEvent data ->
            optionsQuestion data

        EditQuestionListEvent data ->
            listQuestion data

        EditQuestionValueEvent data ->
            valueQuestion data

        EditQuestionIntegrationEvent data ->
            integrationQuestion data

        EditQuestionMultiChoiceEvent data ->
            multiChoiceQuestion data
