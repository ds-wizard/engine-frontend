module Shared.Data.KnowledgeModel.Question exposing
    ( Question(..)
    , addAnswerUuid
    , addChoiceUuid
    , addExpertUuid
    , addItemTemplateQuestionUuids
    , addReferenceUuid
    , decoder
    , getAnnotations
    , getAnswerUuids
    , getChoiceUuids
    , getExpertUuids
    , getFileTypes
    , getIntegrationUuid
    , getItemTemplateQuestionUuids
    , getListQuestionUuid
    , getMaxSize
    , getPropValue
    , getProps
    , getReferenceUuids
    , getRequiredPhaseUuid
    , getTagUuids
    , getText
    , getTitle
    , getTypeString
    , getUuid
    , getValidations
    , getValueType
    , isDesirable
    , isList
    , isMultiChoice
    , isOptions
    , removeAnswerUuid
    , removeChoiceUuid
    , removeExpertUuid
    , removeItemTemplateQuestionUuids
    , removeReferenceUuid
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import List.Extra as List
import Shared.Data.KnowledgeModel.Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Question.CommonQuestionData as CommonQuestionData exposing (CommonQuestionData)
import Shared.Data.KnowledgeModel.Question.FileQuestionData as FileQuestionData exposing (FileQuestionData)
import Shared.Data.KnowledgeModel.Question.IntegrationQuestionData as IntegrationQuestionData exposing (IntegrationQuestionData)
import Shared.Data.KnowledgeModel.Question.ItemSelectQuestionData as ItemSelectQuestionData exposing (ItemSelectQuestionData)
import Shared.Data.KnowledgeModel.Question.ListQuestionData as ListQuestionData exposing (ListQuestionData)
import Shared.Data.KnowledgeModel.Question.MultiChoiceQuestionData as MultiChoiceQuestionData exposing (MultiChoiceQuestionData)
import Shared.Data.KnowledgeModel.Question.OptionsQuestionData as OptionsQuestionData exposing (OptionsQuestionData)
import Shared.Data.KnowledgeModel.Question.QuestionType as QuestionType exposing (QuestionType(..))
import Shared.Data.KnowledgeModel.Question.QuestionValidation exposing (QuestionValidation)
import Shared.Data.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType)
import Shared.Data.KnowledgeModel.Question.ValueQuestionData as ValueQuestionData exposing (ValueQuestionData)


type Question
    = OptionsQuestion CommonQuestionData OptionsQuestionData
    | ListQuestion CommonQuestionData ListQuestionData
    | ValueQuestion CommonQuestionData ValueQuestionData
    | IntegrationQuestion CommonQuestionData IntegrationQuestionData
    | MultiChoiceQuestion CommonQuestionData MultiChoiceQuestionData
    | ItemSelectQuestion CommonQuestionData ItemSelectQuestionData
    | FileQuestion CommonQuestionData FileQuestionData



-- Decoders


decoder : Decoder Question
decoder =
    D.oneOf
        [ D.when QuestionType.decoder ((==) OptionsQuestionType) optionsQuestionDecoder
        , D.when QuestionType.decoder ((==) ListQuestionType) listQuestionDecoder
        , D.when QuestionType.decoder ((==) ValueQuestionType) valueQuestionDecoder
        , D.when QuestionType.decoder ((==) IntegrationQuestionType) integrationQuestionDecoder
        , D.when QuestionType.decoder ((==) MultiChoiceQuestionType) multiChoiceQuestionDecoder
        , D.when QuestionType.decoder ((==) ItemSelectQuestionType) itemSelectQuestionDecoder
        , D.when QuestionType.decoder ((==) FileQuestionType) fileQuestionDecoder
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


itemSelectQuestionDecoder : Decoder Question
itemSelectQuestionDecoder =
    D.map2 ItemSelectQuestion CommonQuestionData.decoder ItemSelectQuestionData.decoder


fileQuestionDecoder : Decoder Question
fileQuestionDecoder =
    D.map2 FileQuestion CommonQuestionData.decoder FileQuestionData.decoder



-- Helpers


addReferenceUuid : String -> Question -> Question
addReferenceUuid referenceUuid =
    mapCommonQuestionData (\c -> { c | referenceUuids = c.referenceUuids ++ [ referenceUuid ] })


removeReferenceUuid : String -> Question -> Question
removeReferenceUuid referenceUuid =
    mapCommonQuestionData (\c -> { c | referenceUuids = List.filter ((/=) referenceUuid) c.referenceUuids })


addExpertUuid : String -> Question -> Question
addExpertUuid expertUuid =
    mapCommonQuestionData (\c -> { c | expertUuids = c.expertUuids ++ [ expertUuid ] })


removeExpertUuid : String -> Question -> Question
removeExpertUuid expertUuid =
    mapCommonQuestionData (\c -> { c | expertUuids = List.filter ((/=) expertUuid) c.expertUuids })


addAnswerUuid : String -> Question -> Question
addAnswerUuid answerUuid question =
    case question of
        OptionsQuestion commonData questionData ->
            OptionsQuestion commonData
                { questionData | answerUuids = questionData.answerUuids ++ [ answerUuid ] }

        _ ->
            question


removeAnswerUuid : String -> Question -> Question
removeAnswerUuid answerUuid question =
    case question of
        OptionsQuestion commonData questionData ->
            OptionsQuestion commonData
                { questionData | answerUuids = List.filter ((/=) answerUuid) questionData.answerUuids }

        _ ->
            question


addItemTemplateQuestionUuids : String -> Question -> Question
addItemTemplateQuestionUuids questionUuid question =
    case question of
        ListQuestion commonData questionData ->
            ListQuestion commonData
                { questionData | itemTemplateQuestionUuids = questionData.itemTemplateQuestionUuids ++ [ questionUuid ] }

        _ ->
            question


removeItemTemplateQuestionUuids : String -> Question -> Question
removeItemTemplateQuestionUuids questionUuid question =
    case question of
        ListQuestion commonData questionData ->
            ListQuestion commonData
                { questionData | itemTemplateQuestionUuids = List.filter ((/=) questionUuid) questionData.itemTemplateQuestionUuids }

        _ ->
            question


addChoiceUuid : String -> Question -> Question
addChoiceUuid choiceUuid question =
    case question of
        MultiChoiceQuestion commonData questionData ->
            MultiChoiceQuestion commonData
                { questionData | choiceUuids = questionData.choiceUuids ++ [ choiceUuid ] }

        _ ->
            question


removeChoiceUuid : String -> Question -> Question
removeChoiceUuid choiceUuid question =
    case question of
        MultiChoiceQuestion commonData questionData ->
            MultiChoiceQuestion commonData
                { questionData | choiceUuids = List.filter ((/=) choiceUuid) questionData.choiceUuids }

        _ ->
            question


mapCommonQuestionData : (CommonQuestionData -> CommonQuestionData) -> Question -> Question
mapCommonQuestionData map question =
    case question of
        OptionsQuestion commonData questionData ->
            OptionsQuestion (map commonData) questionData

        ListQuestion commonData questionData ->
            ListQuestion (map commonData) questionData

        ValueQuestion commonData questionData ->
            ValueQuestion (map commonData) questionData

        IntegrationQuestion commonData questionData ->
            IntegrationQuestion (map commonData) questionData

        MultiChoiceQuestion commonData questionData ->
            MultiChoiceQuestion (map commonData) questionData

        ItemSelectQuestion commonData questionData ->
            ItemSelectQuestion (map commonData) questionData

        FileQuestion commonData questionData ->
            FileQuestion (map commonData) questionData


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

        ItemSelectQuestion data _ ->
            data

        FileQuestion data _ ->
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

        ItemSelectQuestion _ _ ->
            "ItemSelect"

        FileQuestion _ _ ->
            "File"


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


getAnnotations : Question -> List Annotation
getAnnotations =
    getCommonQuestionData >> .annotations


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


getItemTemplateQuestionUuids : Question -> List String
getItemTemplateQuestionUuids question =
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


getValidations : Question -> Maybe (List QuestionValidation)
getValidations question =
    case question of
        ValueQuestion _ data ->
            Just data.validations

        _ ->
            Nothing


getIntegrationUuid : Question -> Maybe String
getIntegrationUuid question =
    case question of
        IntegrationQuestion _ data ->
            Just data.integrationUuid

        _ ->
            Nothing


getMaxSize : Question -> Maybe Int
getMaxSize question =
    case question of
        FileQuestion _ data ->
            data.maxSize

        _ ->
            Nothing


getFileTypes : Question -> Maybe String
getFileTypes question =
    case question of
        FileQuestion _ data ->
            data.fileTypes

        _ ->
            Nothing


getProps : Question -> Maybe (Dict String String)
getProps question =
    case question of
        IntegrationQuestion _ data ->
            Just data.props

        _ ->
            Nothing


getPropValue : String -> Question -> Maybe String
getPropValue prop question =
    case question of
        IntegrationQuestion _ data ->
            Dict.get prop data.props

        _ ->
            Nothing


getListQuestionUuid : Question -> Maybe String
getListQuestionUuid question =
    case question of
        ItemSelectQuestion _ data ->
            data.listQuestionUuid

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
