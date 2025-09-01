module Wizard.Api.Models.Event.EditQuestionEventData exposing
    ( EditQuestionEventData(..)
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
import Wizard.Api.Models.Event.EditQuestionFileEventData as EditQuestionFileEventData exposing (EditQuestionFileEventData)
import Wizard.Api.Models.Event.EditQuestionIntegrationEventData as EditQuestionIntegrationEventData exposing (EditQuestionIntegrationEventData)
import Wizard.Api.Models.Event.EditQuestionItemSelectData as EditQuestionItemSelectEventData exposing (EditQuestionItemSelectEventData)
import Wizard.Api.Models.Event.EditQuestionListEventData as EditQuestionListEventData exposing (EditQuestionListEventData)
import Wizard.Api.Models.Event.EditQuestionMultiChoiceEventData as EditQuestionMultiChoiceEventData exposing (EditQuestionMultiChoiceEventData)
import Wizard.Api.Models.Event.EditQuestionOptionsEventData as EditQuestionOptionsEventData exposing (EditQuestionOptionsEventData)
import Wizard.Api.Models.Event.EditQuestionValueEventData as EditQuestionValueEventData exposing (EditQuestionValueEventData)
import Wizard.Api.Models.Event.EventField as EventField
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValueType as QuestionValueType


type EditQuestionEventData
    = EditQuestionOptionsEvent EditQuestionOptionsEventData
    | EditQuestionListEvent EditQuestionListEventData
    | EditQuestionValueEvent EditQuestionValueEventData
    | EditQuestionIntegrationEvent EditQuestionIntegrationEventData
    | EditQuestionMultiChoiceEvent EditQuestionMultiChoiceEventData
    | EditQuestionItemSelectEvent EditQuestionItemSelectEventData
    | EditQuestionFileEvent EditQuestionFileEventData


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

                    "ItemSelectQuestion" ->
                        D.map EditQuestionItemSelectEvent EditQuestionItemSelectEventData.decoder

                    "FileQuestion" ->
                        D.map EditQuestionFileEvent EditQuestionFileEventData.decoder

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
                EditQuestionItemSelectEventData.encode
                EditQuestionFileEventData.encode
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
                , validations = EventField.getValueWithDefault eventData.validations (Maybe.withDefault [] (Question.getValidations question))
                }

        EditQuestionIntegrationEvent eventData ->
            IntegrationQuestion
                (applyCommonData eventData)
                { integrationUuid = EventField.getValueWithDefault eventData.integrationUuid (Maybe.withDefault "" (Question.getIntegrationUuid question))
                , variables = EventField.getValueWithDefault eventData.variables (Maybe.withDefault Dict.empty (Question.getVariables question))
                }

        EditQuestionMultiChoiceEvent eventData ->
            MultiChoiceQuestion
                (applyCommonData eventData)
                { choiceUuids = EventField.applyChildren eventData.choiceUuids (Question.getChoiceUuids question)
                }

        EditQuestionItemSelectEvent eventData ->
            ItemSelectQuestion
                (applyCommonData eventData)
                { listQuestionUuid = EventField.getValueWithDefault eventData.listQuestionUuid (Question.getListQuestionUuid question)
                }

        EditQuestionFileEvent eventData ->
            FileQuestion
                (applyCommonData eventData)
                { maxSize = EventField.getValueWithDefault eventData.maxSize (Question.getMaxSize question)
                , fileTypes = EventField.getValueWithDefault eventData.fileTypes (Question.getFileTypes question)
                }


getTypeString : EditQuestionEventData -> String
getTypeString =
    map
        (always "Options")
        (always "List")
        (always "Value")
        (always "Integration")
        (always "MultiChoice")
        (always "ItemSelect")
        (always "File")


getEntityVisibleName : EditQuestionEventData -> Maybe String
getEntityVisibleName =
    EventField.getValue << map .title .title .title .title .title .title .title


map :
    (EditQuestionOptionsEventData -> a)
    -> (EditQuestionListEventData -> a)
    -> (EditQuestionValueEventData -> a)
    -> (EditQuestionIntegrationEventData -> a)
    -> (EditQuestionMultiChoiceEventData -> a)
    -> (EditQuestionItemSelectEventData -> a)
    -> (EditQuestionFileEventData -> a)
    -> EditQuestionEventData
    -> a
map optionsQuestion listQuestion valueQuestion integrationQuestion multiChoiceQuestion itemSelectQuestion fileQuestion question =
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

        EditQuestionItemSelectEvent data ->
            itemSelectQuestion data

        EditQuestionFileEvent data ->
            fileQuestion data


squash : EditQuestionEventData -> EditQuestionEventData -> EditQuestionEventData
squash old new =
    case ( old, new ) of
        ( EditQuestionOptionsEvent oldData, EditQuestionOptionsEvent newData ) ->
            EditQuestionOptionsEvent (EditQuestionOptionsEventData.squash oldData newData)

        ( EditQuestionListEvent oldData, EditQuestionListEvent newData ) ->
            EditQuestionListEvent (EditQuestionListEventData.squash oldData newData)

        ( EditQuestionValueEvent oldData, EditQuestionValueEvent newData ) ->
            EditQuestionValueEvent (EditQuestionValueEventData.squash oldData newData)

        ( EditQuestionIntegrationEvent oldData, EditQuestionIntegrationEvent newData ) ->
            EditQuestionIntegrationEvent (EditQuestionIntegrationEventData.squash oldData newData)

        ( EditQuestionMultiChoiceEvent oldData, EditQuestionMultiChoiceEvent newData ) ->
            EditQuestionMultiChoiceEvent (EditQuestionMultiChoiceEventData.squash oldData newData)

        ( EditQuestionItemSelectEvent oldData, EditQuestionItemSelectEvent newData ) ->
            EditQuestionItemSelectEvent (EditQuestionItemSelectEventData.squash oldData newData)

        ( EditQuestionFileEvent oldData, EditQuestionFileEvent newData ) ->
            EditQuestionFileEvent (EditQuestionFileEventData.squash oldData newData)

        _ ->
            new
