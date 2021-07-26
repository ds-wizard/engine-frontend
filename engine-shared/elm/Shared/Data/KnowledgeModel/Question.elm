module Shared.Data.KnowledgeModel.Question exposing
    ( Question(..)
    , decoder
    , getAnswerUuids
    , getChoiceUuids
    , getExpertUuids
    , getIntegrationUuid
    , getItemQuestionUuids
    , getProps
    , getReferenceUuids
    , getRequiredPhaseUuid
    , getTagUuids
    , getText
    , getTitle
    , getTypeString
    , getUuid
    , getValueType
    , isDesirable
    , isList
    , isMultiChoice
    , isOptions
    , new
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Data.BootstrapConfig exposing (BootstrapConfig)
import Shared.Data.KnowledgeModel.Question.CommonQuestionData as CommonQuestionData exposing (CommonQuestionData)
import Shared.Data.KnowledgeModel.Question.IntegrationQuestionData as IntegrationQuestionData exposing (IntegrationQuestionData)
import Shared.Data.KnowledgeModel.Question.ListQuestionData as ListQuestionData exposing (ListQuestionData)
import Shared.Data.KnowledgeModel.Question.MultiChoiceQuestionData as MultiChoiceQuestionData exposing (MultiChoiceQuestionData)
import Shared.Data.KnowledgeModel.Question.OptionsQuestionData as OptionsQuestionData exposing (OptionsQuestionData)
import Shared.Data.KnowledgeModel.Question.QuestionType as QuestionType exposing (QuestionType(..))
import Shared.Data.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType)
import Shared.Data.KnowledgeModel.Question.ValueQuestionData as ValueQuestionData exposing (ValueQuestionData)


type Question
    = OptionsQuestion CommonQuestionData OptionsQuestionData
    | ListQuestion CommonQuestionData ListQuestionData
    | ValueQuestion CommonQuestionData ValueQuestionData
    | IntegrationQuestion CommonQuestionData IntegrationQuestionData
    | MultiChoiceQuestion CommonQuestionData MultiChoiceQuestionData


new : String -> Question
new uuid =
    OptionsQuestion (CommonQuestionData.new uuid) OptionsQuestionData.new



-- Decoders


decoder : Decoder Question
decoder =
    D.oneOf
        [ D.when QuestionType.decoder ((==) OptionsQuestionType) optionsQuestionDecoder
        , D.when QuestionType.decoder ((==) ListQuestionType) listQuestionDecoder
        , D.when QuestionType.decoder ((==) ValueQuestionType) valueQuestionDecoder
        , D.when QuestionType.decoder ((==) IntegrationQuestionType) integrationQuestionDecoder
        , D.when QuestionType.decoder ((==) MultiChoiceQuestionType) multiChoiceQuestionDecoder
        ]


optionsQuestionDecoder : Decoder Question
optionsQuestionDecoder =
    D.map2 OptionsQuestion CommonQuestionData.decoder OptionsQuestionData.decoder


listQuestionDecoder : Decoder Question
listQuestionDecoder =
    D.map2 ListQuestion CommonQuestionData.decoder ListQuestionData.decoder


valueQuestionDecoder : Decoder Question
valueQuestionDecoder =
    D.map2 ValueQuestion CommonQuestionData.decoder ValueQuestionData.decoder


integrationQuestionDecoder : Decoder Question
integrationQuestionDecoder =
    D.map2 IntegrationQuestion CommonQuestionData.decoder IntegrationQuestionData.decoder


multiChoiceQuestionDecoder : Decoder Question
multiChoiceQuestionDecoder =
    D.map2 MultiChoiceQuestion CommonQuestionData.decoder MultiChoiceQuestionData.decoder



-- Helpers


getCommonQuestionData : Question -> CommonQuestionData
getCommonQuestionData question =
    case question of
        OptionsQuestion data _ ->
            data

        ListQuestion data _ ->
            data

        ValueQuestion data _ ->
            data

        IntegrationQuestion data _ ->
            data

        MultiChoiceQuestion data _ ->
            data


getUuid : Question -> String
getUuid =
    getCommonQuestionData >> .uuid


getTitle : Question -> String
getTitle =
    getCommonQuestionData >> .title


getText : Question -> Maybe String
getText =
    getCommonQuestionData >> .text


getTypeString : Question -> String
getTypeString question =
    case question of
        OptionsQuestion _ _ ->
            "Options"

        ListQuestion _ _ ->
            "List"

        ValueQuestion _ _ ->
            "Value"

        IntegrationQuestion _ _ ->
            "Integration"

        MultiChoiceQuestion _ _ ->
            "MultiChoice"


getRequiredPhaseUuid : Question -> Maybe String
getRequiredPhaseUuid =
    getCommonQuestionData >> .requiredPhaseUuid


getTagUuids : Question -> List String
getTagUuids =
    getCommonQuestionData >> .tagUuids


getExpertUuids : Question -> List String
getExpertUuids =
    getCommonQuestionData >> .expertUuids


getReferenceUuids : Question -> List String
getReferenceUuids =
    getCommonQuestionData >> .referenceUuids


getAnswerUuids : Question -> List String
getAnswerUuids question =
    case question of
        OptionsQuestion _ data ->
            data.answerUuids

        _ ->
            []


getChoiceUuids : Question -> List String
getChoiceUuids question =
    case question of
        MultiChoiceQuestion _ data ->
            data.choiceUuids

        _ ->
            []


getItemQuestionUuids : Question -> List String
getItemQuestionUuids question =
    case question of
        ListQuestion _ data ->
            data.itemTemplateQuestionUuids

        _ ->
            []


getValueType : Question -> Maybe QuestionValueType
getValueType question =
    case question of
        ValueQuestion _ data ->
            Just data.valueType

        _ ->
            Nothing


getIntegrationUuid : Question -> Maybe String
getIntegrationUuid question =
    case question of
        IntegrationQuestion _ data ->
            Just data.integrationUuid

        _ ->
            Nothing


getProps : Question -> Maybe (Dict String String)
getProps question =
    case question of
        IntegrationQuestion _ data ->
            Just data.props

        _ ->
            Nothing


isOptions : Question -> Bool
isOptions question =
    case question of
        OptionsQuestion _ _ ->
            True

        _ ->
            False


isMultiChoice : Question -> Bool
isMultiChoice question =
    case question of
        MultiChoiceQuestion _ _ ->
            True

        _ ->
            False


isList : Question -> Bool
isList question =
    case question of
        ListQuestion _ _ ->
            True

        _ ->
            False



-- Utils


isDesirable : List String -> String -> Question -> Bool
isDesirable phases currentPhase question =
    let
        currentPhaseListLength =
            List.length <| List.takeWhile ((/=) currentPhase) phases

        questionPhaseListLength =
            List.length <| List.takeWhile ((/=) (Maybe.withDefault "" (getRequiredPhaseUuid question))) phases
    in
    currentPhaseListLength >= questionPhaseListLength
