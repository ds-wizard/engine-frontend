module Wizard.Api.Models.KnowledgeModel exposing
    ( KnowledgeModel
    , ParentMap
    , createParentMap
    , decoder
    , empty
    , filterWithTags
    , getAllNestedQuestionsByChapter
    , getAllQuestions
    , getAllResourcePages
    , getAnswer
    , getAnswerFollowupQuestions
    , getAnswerName
    , getChapter
    , getChapterName
    , getChapterQuestions
    , getChapters
    , getChoice
    , getChoiceName
    , getExpert
    , getExpertName
    , getIntegration
    , getIntegrationName
    , getIntegrations
    , getMetric
    , getMetricName
    , getMetrics
    , getParent
    , getPhase
    , getPhaseName
    , getPhases
    , getQuestion
    , getQuestionAnswers
    , getQuestionChoices
    , getQuestionExperts
    , getQuestionItemTemplateQuestions
    , getQuestionName
    , getQuestionReferences
    , getReference
    , getReferenceName
    , getResourceCollection
    , getResourceCollectionByResourcePageUuid
    , getResourceCollectionName
    , getResourceCollectionResourcePages
    , getResourceCollectionUuidByResourcePageUuid
    , getResourceCollections
    , getResourcePage
    , getResourcePageName
    , getTag
    , getTagName
    , getTags
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
    , updateChapter
    , updateChoice
    , updateExpert
    , updateIntegration
    , updateMetric
    , updatePhase
    , updateQuestion
    , updateReference
    , updateResourceCollection
    , updateResourcePage
    , updateTag
    )

import Dict exposing (Dict)
import Flip exposing (flip)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Wizard.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration)
import Wizard.Api.Models.KnowledgeModel.KnowledgeModelEntities as KnowledgeModelEntities exposing (KnowledgeModelEntities)
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Wizard.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference)
import Wizard.Api.Models.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Wizard.Api.Models.KnowledgeModel.ResourcePage exposing (ResourcePage)
import Wizard.Api.Models.KnowledgeModel.Tag exposing (Tag)


type alias KnowledgeModel =
    { uuid : Uuid
    , chapterUuids : List String
    , tagUuids : List String
    , integrationUuids : List String
    , metricUuids : List String
    , phaseUuids : List String
    , resourceCollectionUuids : List String
    , entities : KnowledgeModelEntities
    , annotations : List Annotation
    }


type alias ParentMap =
    Dict String String


decoder : Decoder KnowledgeModel
decoder =
    D.succeed KnowledgeModel
        |> D.required "uuid" Uuid.decoder
        |> D.required "chapterUuids" (D.list D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "integrationUuids" (D.list D.string)
        |> D.required "metricUuids" (D.list D.string)
        |> D.required "phaseUuids" (D.list D.string)
        |> D.required "resourceCollectionUuids" (D.list D.string)
        |> D.required "entities" KnowledgeModelEntities.decoder
        |> D.required "annotations" (D.list Annotation.decoder)


empty : KnowledgeModel
empty =
    { uuid = Uuid.nil
    , chapterUuids = []
    , tagUuids = []
    , integrationUuids = []
    , metricUuids = []
    , phaseUuids = []
    , resourceCollectionUuids = []
    , entities = KnowledgeModelEntities.empty
    , annotations = []
    }



-- Insert entities


insertAnswer : Answer -> String -> KnowledgeModel -> KnowledgeModel
insertAnswer answer parentUuid km =
    { km | entities = KnowledgeModelEntities.insertAnswer answer parentUuid km.entities }


insertChapter : Chapter -> String -> KnowledgeModel -> KnowledgeModel
insertChapter chapter _ km =
    { km
        | chapterUuids = km.chapterUuids ++ [ chapter.uuid ]
        , entities = KnowledgeModelEntities.insertChapter chapter km.entities
    }


insertChoice : Choice -> String -> KnowledgeModel -> KnowledgeModel
insertChoice choice parentUuid km =
    { km | entities = KnowledgeModelEntities.insertChoice choice parentUuid km.entities }


insertExpert : Expert -> String -> KnowledgeModel -> KnowledgeModel
insertExpert expert parentUuid km =
    { km | entities = KnowledgeModelEntities.insertExpert expert parentUuid km.entities }


insertIntegration : Integration -> String -> KnowledgeModel -> KnowledgeModel
insertIntegration integration _ km =
    { km
        | integrationUuids = km.integrationUuids ++ [ Integration.getUuid integration ]
        , entities = KnowledgeModelEntities.insertIntegration integration km.entities
    }


insertMetric : Metric -> String -> KnowledgeModel -> KnowledgeModel
insertMetric metric _ km =
    { km
        | metricUuids = km.metricUuids ++ [ metric.uuid ]
        , entities = KnowledgeModelEntities.insertMetric metric km.entities
    }


insertPhase : Phase -> String -> KnowledgeModel -> KnowledgeModel
insertPhase phase _ km =
    { km
        | phaseUuids = km.phaseUuids ++ [ phase.uuid ]
        , entities = KnowledgeModelEntities.insertPhase phase km.entities
    }


insertQuestion : Question -> String -> KnowledgeModel -> KnowledgeModel
insertQuestion question parentUuid km =
    { km | entities = KnowledgeModelEntities.insertQuestion question parentUuid km.entities }


insertReference : Reference -> String -> KnowledgeModel -> KnowledgeModel
insertReference reference parentUuid km =
    { km | entities = KnowledgeModelEntities.insertReference reference parentUuid km.entities }


insertResourceCollection : ResourceCollection -> String -> KnowledgeModel -> KnowledgeModel
insertResourceCollection resourceCollection _ km =
    { km
        | resourceCollectionUuids = km.resourceCollectionUuids ++ [ resourceCollection.uuid ]
        , entities = KnowledgeModelEntities.insertResourceCollection resourceCollection km.entities
    }


insertResourcePage : ResourcePage -> String -> KnowledgeModel -> KnowledgeModel
insertResourcePage resourcePage parentUuid km =
    { km | entities = KnowledgeModelEntities.insertResourcePage resourcePage parentUuid km.entities }


insertTag : Tag -> String -> KnowledgeModel -> KnowledgeModel
insertTag tag _ km =
    { km
        | tagUuids = km.tagUuids ++ [ tag.uuid ]
        , entities = KnowledgeModelEntities.insertTag tag km.entities
    }



-- Update entities


updateAnswer : Answer -> KnowledgeModel -> KnowledgeModel
updateAnswer answer km =
    { km | entities = KnowledgeModelEntities.updateAnswer answer km.entities }


updateChapter : Chapter -> KnowledgeModel -> KnowledgeModel
updateChapter chapter km =
    { km | entities = KnowledgeModelEntities.insertChapter chapter km.entities }


updateChoice : Choice -> KnowledgeModel -> KnowledgeModel
updateChoice choice km =
    { km | entities = KnowledgeModelEntities.updateChoice choice km.entities }


updateExpert : Expert -> KnowledgeModel -> KnowledgeModel
updateExpert expert km =
    { km | entities = KnowledgeModelEntities.updateExpert expert km.entities }


updateIntegration : Integration -> KnowledgeModel -> KnowledgeModel
updateIntegration integration km =
    { km | entities = KnowledgeModelEntities.insertIntegration integration km.entities }


updateMetric : Metric -> KnowledgeModel -> KnowledgeModel
updateMetric metric km =
    { km | entities = KnowledgeModelEntities.insertMetric metric km.entities }


updatePhase : Phase -> KnowledgeModel -> KnowledgeModel
updatePhase phase km =
    { km | entities = KnowledgeModelEntities.insertPhase phase km.entities }


updateQuestion : Question -> KnowledgeModel -> KnowledgeModel
updateQuestion question km =
    { km | entities = KnowledgeModelEntities.updateQuestion question km.entities }


updateReference : Reference -> KnowledgeModel -> KnowledgeModel
updateReference reference km =
    { km | entities = KnowledgeModelEntities.updateReference reference km.entities }


updateResourceCollection : ResourceCollection -> KnowledgeModel -> KnowledgeModel
updateResourceCollection resourceCollection km =
    { km | entities = KnowledgeModelEntities.updateResourceCollection resourceCollection km.entities }


updateResourcePage : ResourcePage -> KnowledgeModel -> KnowledgeModel
updateResourcePage resourcePage km =
    { km | entities = KnowledgeModelEntities.updateResourcePage resourcePage km.entities }


updateTag : Tag -> KnowledgeModel -> KnowledgeModel
updateTag tag km =
    { km | entities = KnowledgeModelEntities.insertTag tag km.entities }



-- Move entities


moveAnswer : Answer -> String -> String -> KnowledgeModel -> KnowledgeModel
moveAnswer answer oldParent newParent km =
    { km | entities = KnowledgeModelEntities.moveAnswer answer oldParent newParent km.entities }


moveChoice : Choice -> String -> String -> KnowledgeModel -> KnowledgeModel
moveChoice choice oldParent newParent km =
    { km | entities = KnowledgeModelEntities.moveChoice choice oldParent newParent km.entities }


moveExpert : Expert -> String -> String -> KnowledgeModel -> KnowledgeModel
moveExpert expert oldParent newParent km =
    { km | entities = KnowledgeModelEntities.moveExpert expert oldParent newParent km.entities }


moveQuestion : Question -> String -> String -> KnowledgeModel -> KnowledgeModel
moveQuestion question oldParent newParent km =
    { km | entities = KnowledgeModelEntities.moveQuestion question oldParent newParent km.entities }


moveReference : Reference -> String -> String -> KnowledgeModel -> KnowledgeModel
moveReference reference oldParent newParent km =
    { km | entities = KnowledgeModelEntities.moveReference reference oldParent newParent km.entities }



-- Direct entity getters


getChapter : String -> KnowledgeModel -> Maybe Chapter
getChapter uuid km =
    Dict.get uuid km.entities.chapters


getMetric : String -> KnowledgeModel -> Maybe Metric
getMetric uuid km =
    Dict.get uuid km.entities.metrics


getPhase : String -> KnowledgeModel -> Maybe Phase
getPhase uuid km =
    Dict.get uuid km.entities.phases


getTag : String -> KnowledgeModel -> Maybe Tag
getTag uuid km =
    Dict.get uuid km.entities.tags


getIntegration : String -> KnowledgeModel -> Maybe Integration
getIntegration uuid km =
    Dict.get uuid km.entities.integrations


getQuestion : String -> KnowledgeModel -> Maybe Question
getQuestion uuid km =
    Dict.get uuid km.entities.questions


getAllQuestions : KnowledgeModel -> List Question
getAllQuestions km =
    Dict.values km.entities.questions


getAnswer : String -> KnowledgeModel -> Maybe Answer
getAnswer uuid km =
    Dict.get uuid km.entities.answers


getChoice : String -> KnowledgeModel -> Maybe Choice
getChoice uuid km =
    Dict.get uuid km.entities.choices


getReference : String -> KnowledgeModel -> Maybe Reference
getReference uuid km =
    Dict.get uuid km.entities.references


getResourceCollection : String -> KnowledgeModel -> Maybe ResourceCollection
getResourceCollection uuid km =
    Dict.get uuid km.entities.resourceCollections


getResourceCollectionUuidByResourcePageUuid : String -> KnowledgeModel -> Maybe String
getResourceCollectionUuidByResourcePageUuid resourcePageUuid km =
    getResourceCollectionByResourcePageUuid resourcePageUuid km
        |> Maybe.map .uuid


getResourceCollectionByResourcePageUuid : String -> KnowledgeModel -> Maybe ResourceCollection
getResourceCollectionByResourcePageUuid resourcePageUuid km =
    Dict.values km.entities.resourceCollections
        |> List.find (\rc -> List.member resourcePageUuid rc.resourcePageUuids)


getResourcePage : String -> KnowledgeModel -> Maybe ResourcePage
getResourcePage uuid km =
    Dict.get uuid km.entities.resourcePages


getAllResourcePages : KnowledgeModel -> List ResourcePage
getAllResourcePages km =
    Dict.values km.entities.resourcePages


getExpert : String -> KnowledgeModel -> Maybe Expert
getExpert uuid km =
    Dict.get uuid km.entities.experts



-- Entity name getters


getChapterName : KnowledgeModel -> String -> String
getChapterName km uuid =
    Maybe.unwrap "" .title <| getChapter uuid km


getMetricName : KnowledgeModel -> String -> String
getMetricName km uuid =
    Maybe.unwrap "" .title <| getMetric uuid km


getPhaseName : KnowledgeModel -> String -> String
getPhaseName km uuid =
    Maybe.unwrap "" .title <| getPhase uuid km


getTagName : KnowledgeModel -> String -> String
getTagName km uuid =
    Maybe.unwrap "" .name <| getTag uuid km


getIntegrationName : KnowledgeModel -> String -> String
getIntegrationName km uuid =
    Maybe.unwrap "" Integration.getVisibleName <| getIntegration uuid km


getQuestionName : KnowledgeModel -> String -> String
getQuestionName km uuid =
    Maybe.unwrap "" Question.getTitle <| getQuestion uuid km


getAnswerName : KnowledgeModel -> String -> String
getAnswerName km uuid =
    Maybe.unwrap "" .label <| getAnswer uuid km


getExpertName : KnowledgeModel -> String -> String
getExpertName km uuid =
    Maybe.unwrap "" .name <| getExpert uuid km


getReferenceName : KnowledgeModel -> String -> String
getReferenceName km uuid =
    Maybe.unwrap "" (Reference.getVisibleName (getAllQuestions km) (getAllResourcePages km)) <| getReference uuid km


getResourceCollectionName : KnowledgeModel -> String -> String
getResourceCollectionName km uuid =
    Maybe.unwrap "" .title <| getResourceCollection uuid km


getResourcePageName : KnowledgeModel -> String -> String
getResourcePageName km uuid =
    Maybe.unwrap "" .title <| getResourcePage uuid km


getChoiceName : KnowledgeModel -> String -> String
getChoiceName km uuid =
    Maybe.unwrap "" .label <| getChoice uuid km



-- Nested entities helpers


getChapters : KnowledgeModel -> List Chapter
getChapters km =
    resolveEntities km.entities.chapters km.chapterUuids


getIntegrations : KnowledgeModel -> List Integration
getIntegrations km =
    resolveEntities km.entities.integrations km.integrationUuids


getMetrics : KnowledgeModel -> List Metric
getMetrics km =
    resolveEntities km.entities.metrics km.metricUuids


getPhases : KnowledgeModel -> List Phase
getPhases km =
    resolveEntities km.entities.phases km.phaseUuids


getResourceCollections : KnowledgeModel -> List ResourceCollection
getResourceCollections km =
    resolveEntities km.entities.resourceCollections km.resourceCollectionUuids


getTags : KnowledgeModel -> List Tag
getTags km =
    resolveEntities km.entities.tags km.tagUuids


getChapterQuestions : String -> KnowledgeModel -> List Question
getChapterQuestions =
    getEntities .chapters .questionUuids .questions


getQuestionAnswers : String -> KnowledgeModel -> List Answer
getQuestionAnswers =
    getEntities .questions Question.getAnswerUuids .answers


getQuestionChoices : String -> KnowledgeModel -> List Choice
getQuestionChoices =
    getEntities .questions Question.getChoiceUuids .choices


getQuestionReferences : String -> KnowledgeModel -> List Reference
getQuestionReferences =
    getEntities .questions Question.getReferenceUuids .references


getQuestionExperts : String -> KnowledgeModel -> List Expert
getQuestionExperts =
    getEntities .questions Question.getExpertUuids .experts


getQuestionItemTemplateQuestions : String -> KnowledgeModel -> List Question
getQuestionItemTemplateQuestions =
    getEntities .questions Question.getItemTemplateQuestionUuids .questions


getAnswerFollowupQuestions : String -> KnowledgeModel -> List Question
getAnswerFollowupQuestions =
    getEntities .answers .followUpUuids .questions


getResourceCollectionResourcePages : String -> KnowledgeModel -> List ResourcePage
getResourceCollectionResourcePages =
    getEntities .resourceCollections .resourcePageUuids .resourcePages


getEntities :
    (KnowledgeModelEntities -> Dict String parent)
    -> (parent -> List String)
    -> (KnowledgeModelEntities -> Dict String entity)
    -> String
    -> KnowledgeModel
    -> List entity
getEntities getParents getChildUuids getChildren uuid km =
    Dict.get uuid (getParents km.entities)
        |> Maybe.map getChildUuids
        |> Maybe.withDefault []
        |> resolveEntities (getChildren km.entities)


resolveEntities : Dict String a -> List String -> List a
resolveEntities entities =
    Maybe.values << List.map (\uuid -> Dict.get uuid entities)


getAllNestedQuestionsByChapter : KnowledgeModel -> List ( Chapter, List Question )
getAllNestedQuestionsByChapter km =
    let
        getNestedQuestions question =
            let
                questionUuid =
                    Question.getUuid question

                followupQuestions =
                    List.concatMap
                        (flip getAnswerFollowupQuestions km << .uuid)
                        (getQuestionAnswers questionUuid km)

                itemTemplateQuestions =
                    getQuestionItemTemplateQuestions questionUuid km
            in
            followupQuestions ++ itemTemplateQuestions

        collectQuestions question =
            question :: List.concatMap collectQuestions (getNestedQuestions question)

        getChapterQuestions_ chapter =
            let
                topLevelQuestions =
                    getChapterQuestions chapter.uuid km
            in
            List.concatMap collectQuestions topLevelQuestions

        chapters =
            getChapters km
    in
    List.map (\chapter -> ( chapter, getChapterQuestions_ chapter )) chapters



-- Other


createParentMap : KnowledgeModel -> ParentMap
createParentMap km =
    let
        insert parentUuid entities dict =
            List.foldl (\e d -> Dict.insert e parentUuid d) dict entities

        processKM km_ dict =
            let
                insert_ getChildUuids =
                    insert (Uuid.toString km_.uuid) (getChildUuids km_)
            in
            dict
                |> insert_ .chapterUuids
                |> insert_ .metricUuids
                |> insert_ .phaseUuids
                |> insert_ .tagUuids
                |> insert_ .integrationUuids
                |> insert_ .resourceCollectionUuids

        processChapter chapter dict =
            let
                insert_ getChildUuids =
                    insert chapter.uuid (getChildUuids chapter)
            in
            dict
                |> insert_ .questionUuids

        processQuestion question dict =
            let
                insert_ getChildUuids =
                    insert (Question.getUuid question) (getChildUuids question)
            in
            dict
                |> insert_ Question.getReferenceUuids
                |> insert_ Question.getExpertUuids
                |> insert_ Question.getAnswerUuids
                |> insert_ Question.getItemTemplateQuestionUuids
                |> insert_ Question.getChoiceUuids

        processAnswer answer dict =
            let
                insert_ getChildUuids =
                    insert answer.uuid (getChildUuids answer)
            in
            dict
                |> insert_ .followUpUuids

        processResourceCollection resourceCollection dict =
            let
                insert_ getChildUuids =
                    insert resourceCollection.uuid (getChildUuids resourceCollection)
            in
            dict
                |> insert_ .resourcePageUuids

        processChapters chapters dict =
            List.foldl processChapter dict <| Dict.values chapters

        processQuestions questions dict =
            List.foldl processQuestion dict <| Dict.values questions

        processAnswers answers dict =
            List.foldl processAnswer dict <| Dict.values answers

        processResourceCollections resourceCollections dict =
            List.foldl processResourceCollection dict <| Dict.values resourceCollections
    in
    Dict.empty
        |> processKM km
        |> processChapters km.entities.chapters
        |> processQuestions km.entities.questions
        |> processAnswers km.entities.answers
        |> processResourceCollections km.entities.resourceCollections


getParent : ParentMap -> String -> String
getParent parentMap uuid =
    Maybe.withDefault (Uuid.toString Uuid.nil) <| Dict.get uuid parentMap


filterWithTags : List String -> KnowledgeModel -> KnowledgeModel
filterWithTags tags km =
    if List.isEmpty tags then
        km

    else
        let
            filterQuestion _ question =
                Question.getTagUuids question
                    |> List.any (\t -> List.member t tags)

            filteredQuestions =
                Dict.filter filterQuestion km.entities.questions

            filterTag _ tag =
                List.member tag.uuid tags

            filteredTags =
                Dict.filter filterTag km.entities.tags
        in
        { km
            | entities =
                km.entities
                    |> KnowledgeModelEntities.updateQuestions filteredQuestions
                    |> KnowledgeModelEntities.updateTags filteredTags
            , tagUuids = tags
        }
