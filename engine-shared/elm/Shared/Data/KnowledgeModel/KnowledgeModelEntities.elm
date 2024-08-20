module Shared.Data.KnowledgeModel.KnowledgeModelEntities exposing
    ( KnowledgeModelEntities
    , decoder
    , empty
    , insertAnswer
    , insertChapter
    , insertChoice
    , insertExpert
    , insertIntegration
    , insertMetric
    , insertPhase
    , insertQuestion
    , insertReference
    , insertResourceCollection
    , insertResourcePage
    , insertTag
    , moveAnswer
    , moveChoice
    , moveExpert
    , moveQuestion
    , moveReference
    , updateAnswer
    , updateChoice
    , updateExpert
    , updateQuestion
    , updateQuestions
    , updateReference
    , updateResourceCollection
    , updateResourcePage
    , updateTags
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Answer as Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter as Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice as Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Expert as Expert exposing (Expert)
import Shared.Data.KnowledgeModel.Integration as Integration exposing (Integration)
import Shared.Data.KnowledgeModel.Metric as Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Phase as Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference)
import Shared.Data.KnowledgeModel.ResourceCollection as ResourceCollection exposing (ResourceCollection)
import Shared.Data.KnowledgeModel.ResourcePage as ResourcePage exposing (ResourcePage)
import Shared.Data.KnowledgeModel.Tag as Tag exposing (Tag)


type alias KnowledgeModelEntities =
    { chapters : Dict String Chapter
    , questions : Dict String Question
    , answers : Dict String Answer
    , choices : Dict String Choice
    , experts : Dict String Expert
    , references : Dict String Reference
    , resourceCollections : Dict String ResourceCollection
    , resourcePages : Dict String ResourcePage
    , integrations : Dict String Integration
    , tags : Dict String Tag
    , metrics : Dict String Metric
    , phases : Dict String Phase
    }


decoder : Decoder KnowledgeModelEntities
decoder =
    D.succeed KnowledgeModelEntities
        |> D.required "chapters" (D.dict Chapter.decoder)
        |> D.required "questions" (D.dict Question.decoder)
        |> D.required "answers" (D.dict Answer.decoder)
        |> D.required "choices" (D.dict Choice.decoder)
        |> D.required "experts" (D.dict Expert.decoder)
        |> D.required "references" (D.dict Reference.decoder)
        |> D.required "resourceCollections" (D.dict ResourceCollection.decoder)
        |> D.required "resourcePages" (D.dict ResourcePage.decoder)
        |> D.required "integrations" (D.dict Integration.decoder)
        |> D.required "tags" (D.dict Tag.decoder)
        |> D.required "metrics" (D.dict Metric.decoder)
        |> D.required "phases" (D.dict Phase.decoder)


empty : KnowledgeModelEntities
empty =
    { chapters = Dict.empty
    , questions = Dict.empty
    , answers = Dict.empty
    , choices = Dict.empty
    , experts = Dict.empty
    , references = Dict.empty
    , resourceCollections = Dict.empty
    , resourcePages = Dict.empty
    , integrations = Dict.empty
    , tags = Dict.empty
    , metrics = Dict.empty
    , phases = Dict.empty
    }


insertAnswer : Answer -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
insertAnswer answer questionUuid entities =
    case Dict.get questionUuid entities.questions of
        Just question ->
            { entities
                | answers = Dict.insert answer.uuid answer entities.answers
                , questions = Dict.insert questionUuid (Question.addAnswerUuid answer.uuid question) entities.questions
            }

        Nothing ->
            entities


insertChapter : Chapter -> KnowledgeModelEntities -> KnowledgeModelEntities
insertChapter chapter entities =
    { entities | chapters = Dict.insert chapter.uuid chapter entities.chapters }


insertChoice : Choice -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
insertChoice choice questionUuid entities =
    case Dict.get questionUuid entities.questions of
        Just question ->
            { entities
                | choices = Dict.insert choice.uuid choice entities.choices
                , questions = Dict.insert questionUuid (Question.addChoiceUuid choice.uuid question) entities.questions
            }

        Nothing ->
            entities


insertExpert : Expert -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
insertExpert expert questionUuid entities =
    case Dict.get questionUuid entities.questions of
        Just question ->
            { entities
                | experts = Dict.insert expert.uuid expert entities.experts
                , questions = Dict.insert questionUuid (Question.addExpertUuid expert.uuid question) entities.questions
            }

        Nothing ->
            entities


insertIntegration : Integration -> KnowledgeModelEntities -> KnowledgeModelEntities
insertIntegration integration entities =
    { entities | integrations = Dict.insert (Integration.getUuid integration) integration entities.integrations }


insertMetric : Metric -> KnowledgeModelEntities -> KnowledgeModelEntities
insertMetric metric entities =
    { entities | metrics = Dict.insert metric.uuid metric entities.metrics }


insertPhase : Phase -> KnowledgeModelEntities -> KnowledgeModelEntities
insertPhase phase entities =
    { entities | phases = Dict.insert phase.uuid phase entities.phases }


insertQuestion : Question -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
insertQuestion question parentUuid entities =
    let
        questionUuid =
            Question.getUuid question
    in
    case Dict.get parentUuid entities.chapters of
        Just chapter ->
            { entities
                | questions = Dict.insert questionUuid question entities.questions
                , chapters = Dict.insert chapter.uuid (Chapter.addQuestionUuid questionUuid chapter) entities.chapters
            }

        Nothing ->
            case Dict.get parentUuid entities.questions of
                Just parentQuestion ->
                    { entities
                        | questions =
                            entities.questions
                                |> Dict.insert questionUuid question
                                |> Dict.insert (Question.getUuid parentQuestion) (Question.addItemTemplateQuestionUuids questionUuid parentQuestion)
                    }

                Nothing ->
                    case Dict.get parentUuid entities.answers of
                        Just answer ->
                            { entities
                                | questions = Dict.insert questionUuid question entities.questions
                                , answers = Dict.insert answer.uuid (Answer.addFollowUpUuid questionUuid answer) entities.answers
                            }

                        Nothing ->
                            entities


insertReference : Reference -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
insertReference reference questionUuid entities =
    case Dict.get questionUuid entities.questions of
        Just question ->
            { entities
                | references = Dict.insert (Reference.getUuid reference) reference entities.references
                , questions = Dict.insert questionUuid (Question.addReferenceUuid (Reference.getUuid reference) question) entities.questions
            }

        Nothing ->
            entities


insertResourceCollection : ResourceCollection -> KnowledgeModelEntities -> KnowledgeModelEntities
insertResourceCollection resourceCollection entities =
    { entities | resourceCollections = Dict.insert resourceCollection.uuid resourceCollection entities.resourceCollections }


insertResourcePage : ResourcePage -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
insertResourcePage resourcePage resourceCollectionUuid entities =
    case Dict.get resourceCollectionUuid entities.resourceCollections of
        Just resourceCollection ->
            { entities
                | resourcePages = Dict.insert resourcePage.uuid resourcePage entities.resourcePages
                , resourceCollections = Dict.insert resourceCollectionUuid (ResourceCollection.addResourcePageUuid resourcePage.uuid resourceCollection) entities.resourceCollections
            }

        Nothing ->
            entities


insertTag : Tag -> KnowledgeModelEntities -> KnowledgeModelEntities
insertTag tag entities =
    { entities | tags = Dict.insert tag.uuid tag entities.tags }


moveAnswer : Answer -> String -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
moveAnswer answer oldParentUuid newParentUuid entities =
    let
        entitiesRemoved =
            case Dict.get oldParentUuid entities.questions of
                Just question ->
                    { entities | questions = Dict.insert (Question.getUuid question) (Question.removeAnswerUuid answer.uuid question) entities.questions }

                Nothing ->
                    entities
    in
    case Dict.get newParentUuid entities.questions of
        Just question ->
            { entitiesRemoved | questions = Dict.insert (Question.getUuid question) (Question.addAnswerUuid answer.uuid question) entitiesRemoved.questions }

        Nothing ->
            entitiesRemoved


moveChoice : Choice -> String -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
moveChoice choice oldParentUuid newParentUuid entities =
    let
        entitiesRemoved =
            case Dict.get oldParentUuid entities.questions of
                Just question ->
                    { entities | questions = Dict.insert (Question.getUuid question) (Question.removeChoiceUuid choice.uuid question) entities.questions }

                Nothing ->
                    entities
    in
    case Dict.get newParentUuid entities.questions of
        Just question ->
            { entitiesRemoved | questions = Dict.insert (Question.getUuid question) (Question.addChoiceUuid choice.uuid question) entitiesRemoved.questions }

        Nothing ->
            entitiesRemoved


moveExpert : Expert -> String -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
moveExpert expert oldParentUuid newParentUuid entities =
    let
        entitiesRemoved =
            case Dict.get oldParentUuid entities.questions of
                Just question ->
                    { entities | questions = Dict.insert (Question.getUuid question) (Question.removeExpertUuid expert.uuid question) entities.questions }

                Nothing ->
                    entities
    in
    case Dict.get newParentUuid entities.questions of
        Just question ->
            { entitiesRemoved | questions = Dict.insert (Question.getUuid question) (Question.addExpertUuid expert.uuid question) entitiesRemoved.questions }

        Nothing ->
            entitiesRemoved


moveQuestion : Question -> String -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
moveQuestion question oldParentUuid newParentUuid entities =
    let
        questionUuid =
            Question.getUuid question

        entitiesRemoved =
            case Dict.get oldParentUuid entities.chapters of
                Just chapter ->
                    { entities | chapters = Dict.insert chapter.uuid (Chapter.removeQuestionUuid questionUuid chapter) entities.chapters }

                Nothing ->
                    case Dict.get oldParentUuid entities.questions of
                        Just parentQuestion ->
                            { entities | questions = Dict.insert (Question.getUuid parentQuestion) (Question.removeItemTemplateQuestionUuids questionUuid parentQuestion) entities.questions }

                        Nothing ->
                            case Dict.get oldParentUuid entities.answers of
                                Just answer ->
                                    { entities | answers = Dict.insert answer.uuid (Answer.removeFollowUpUuid questionUuid answer) entities.answers }

                                Nothing ->
                                    entities
    in
    case Dict.get newParentUuid entities.chapters of
        Just chapter ->
            { entitiesRemoved | chapters = Dict.insert chapter.uuid (Chapter.addQuestionUuid questionUuid chapter) entitiesRemoved.chapters }

        Nothing ->
            case Dict.get newParentUuid entities.questions of
                Just parentQuestion ->
                    { entitiesRemoved | questions = Dict.insert (Question.getUuid parentQuestion) (Question.addItemTemplateQuestionUuids questionUuid parentQuestion) entitiesRemoved.questions }

                Nothing ->
                    case Dict.get newParentUuid entities.answers of
                        Just answer ->
                            { entitiesRemoved | answers = Dict.insert answer.uuid (Answer.addFollowUpUuid questionUuid answer) entitiesRemoved.answers }

                        Nothing ->
                            entitiesRemoved


moveReference : Reference -> String -> String -> KnowledgeModelEntities -> KnowledgeModelEntities
moveReference reference oldParentUuid newParentUuid entities =
    let
        referenceUuid =
            Reference.getUuid reference

        entitiesRemoved =
            case Dict.get oldParentUuid entities.questions of
                Just question ->
                    { entities | questions = Dict.insert (Question.getUuid question) (Question.removeReferenceUuid referenceUuid question) entities.questions }

                Nothing ->
                    entities
    in
    case Dict.get newParentUuid entities.questions of
        Just question ->
            { entitiesRemoved | questions = Dict.insert (Question.getUuid question) (Question.addReferenceUuid referenceUuid question) entitiesRemoved.questions }

        Nothing ->
            entitiesRemoved


updateAnswer : Answer -> KnowledgeModelEntities -> KnowledgeModelEntities
updateAnswer answer entities =
    { entities | answers = Dict.insert answer.uuid answer entities.answers }


updateChoice : Choice -> KnowledgeModelEntities -> KnowledgeModelEntities
updateChoice choice entities =
    { entities | choices = Dict.insert choice.uuid choice entities.choices }


updateExpert : Expert -> KnowledgeModelEntities -> KnowledgeModelEntities
updateExpert expert entities =
    { entities | experts = Dict.insert expert.uuid expert entities.experts }


updateQuestion : Question -> KnowledgeModelEntities -> KnowledgeModelEntities
updateQuestion question entities =
    { entities | questions = Dict.insert (Question.getUuid question) question entities.questions }


updateReference : Reference -> KnowledgeModelEntities -> KnowledgeModelEntities
updateReference reference entities =
    { entities | references = Dict.insert (Reference.getUuid reference) reference entities.references }


updateResourceCollection : ResourceCollection -> KnowledgeModelEntities -> KnowledgeModelEntities
updateResourceCollection resourceCollection entities =
    { entities | resourceCollections = Dict.insert resourceCollection.uuid resourceCollection entities.resourceCollections }


updateResourcePage : ResourcePage -> KnowledgeModelEntities -> KnowledgeModelEntities
updateResourcePage resourcePage entities =
    { entities | resourcePages = Dict.insert resourcePage.uuid resourcePage entities.resourcePages }



--


updateQuestions : Dict String Question -> KnowledgeModelEntities -> KnowledgeModelEntities
updateQuestions newQuestions entities =
    { entities | questions = newQuestions }


updateTags : Dict String Tag -> KnowledgeModelEntities -> KnowledgeModelEntities
updateTags newTags entities =
    { entities | tags = newTags }
