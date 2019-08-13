module KMEditor.Common.KnowledgeModel.KnowledgeModel exposing
    ( KnowledgeModel
    , createParentMap
    , decoder
    , filterWithTags
    , getAllQuestions
    , getAnswer
    , getAnswerFollowupQuestions
    , getChapter
    , getChapterQuestions
    , getChapters
    , getExpert
    , getIntegration
    , getIntegrations
    , getParent
    , getQuestion
    , getQuestionAnswers
    , getQuestionExperts
    , getQuestionItemTemplateQuestions
    , getQuestionReferences
    , getReference
    , getTag
    , getTags
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import KMEditor.Common.KnowledgeModel.Answer exposing (Answer)
import KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import KMEditor.Common.KnowledgeModel.Expert exposing (Expert)
import KMEditor.Common.KnowledgeModel.Integration exposing (Integration)
import KMEditor.Common.KnowledgeModel.KnowledgeModelEntities as KnowledgeModelEntities exposing (KnowledgeModelEntities)
import KMEditor.Common.KnowledgeModel.Question as Question exposing (Question)
import KMEditor.Common.KnowledgeModel.Reference exposing (Reference)
import KMEditor.Common.KnowledgeModel.Tag exposing (Tag)
import Utils exposing (nilUuid)


type alias KnowledgeModel =
    { uuid : String
    , name : String
    , chapterUuids : List String
    , tagUuids : List String
    , integrationUuids : List String
    , entities : KnowledgeModelEntities
    }


type alias ParentMap =
    Dict String String


decoder : Decoder KnowledgeModel
decoder =
    D.succeed KnowledgeModel
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "chapterUuids" (D.list D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "integrationUuids" (D.list D.string)
        |> D.required "entities" KnowledgeModelEntities.decoder



-- Direct entity getters


getChapter : String -> KnowledgeModel -> Maybe Chapter
getChapter uuid km =
    Dict.get uuid km.entities.chapters


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


getReference : String -> KnowledgeModel -> Maybe Reference
getReference uuid km =
    Dict.get uuid km.entities.references


getExpert : String -> KnowledgeModel -> Maybe Expert
getExpert uuid km =
    Dict.get uuid km.entities.experts



-- Nested entities helpers


getChapters : KnowledgeModel -> List Chapter
getChapters km =
    resolveEntities km.entities.chapters km.chapterUuids


getIntegrations : KnowledgeModel -> List Integration
getIntegrations km =
    resolveEntities km.entities.integrations km.integrationUuids


getTags : KnowledgeModel -> List Tag
getTags km =
    resolveEntities km.entities.tags km.tagUuids


getChapterQuestions : String -> KnowledgeModel -> List Question
getChapterQuestions =
    getEntities .chapters .questionUuids resolveQuestions


getQuestionAnswers : String -> KnowledgeModel -> List Answer
getQuestionAnswers =
    getEntities .questions Question.getAnswerUuids resolveAnswers


getQuestionReferences : String -> KnowledgeModel -> List Reference
getQuestionReferences =
    getEntities .questions Question.getReferenceUuids resolveReferences


getQuestionExperts : String -> KnowledgeModel -> List Expert
getQuestionExperts =
    getEntities .questions Question.getExpertUuids resolveExperts


getQuestionItemTemplateQuestions : String -> KnowledgeModel -> List Question
getQuestionItemTemplateQuestions =
    getEntities .questions Question.getItemQuestionUuids resolveQuestions


getAnswerFollowupQuestions : String -> KnowledgeModel -> List Question
getAnswerFollowupQuestions =
    getEntities .answers .followUpUuids resolveQuestions


getEntities :
    (KnowledgeModelEntities -> Dict String parent)
    -> (parent -> List String)
    -> (KnowledgeModel -> List String -> List entity)
    -> String
    -> KnowledgeModel
    -> List entity
getEntities getParents getChildUuids resolveFn uuid km =
    Dict.get uuid (getParents km.entities)
        |> Maybe.map (\parent -> resolveFn km (getChildUuids parent))
        |> Maybe.withDefault []


resolveAnswers : KnowledgeModel -> List String -> List Answer
resolveAnswers km =
    resolveEntities km.entities.answers


resolveQuestions : KnowledgeModel -> List String -> List Question
resolveQuestions km =
    resolveEntities km.entities.questions


resolveExperts : KnowledgeModel -> List String -> List Expert
resolveExperts km =
    resolveEntities km.entities.experts


resolveReferences : KnowledgeModel -> List String -> List Reference
resolveReferences km =
    resolveEntities km.entities.references


resolveEntities : Dict String a -> List String -> List a
resolveEntities entities uuids =
    Dict.values <| Dict.filter (\uuid _ -> List.member uuid uuids) entities



-- Other


createParentMap : KnowledgeModel -> ParentMap
createParentMap km =
    let
        insert parentUuid entities dict =
            List.foldl (\e d -> Dict.insert e parentUuid d) dict entities

        processKM km_ dict =
            let
                insert_ getChildUuids =
                    insert km_.uuid (getChildUuids km_)
            in
            dict
                |> insert_ .chapterUuids
                |> insert_ .tagUuids
                |> insert_ .integrationUuids

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
                |> insert_ Question.getItemQuestionUuids

        processAnswer answer dict =
            let
                insert_ getChildUuids =
                    insert answer.uuid (getChildUuids answer)
            in
            dict
                |> insert_ .followUpUuids

        processChapters chapters dict =
            List.foldl processChapter dict <| Dict.values chapters

        processQuestions questions dict =
            List.foldl processQuestion dict <| Dict.values questions

        processAnswers answers dict =
            List.foldl processAnswer dict <| Dict.values answers
    in
    Dict.empty
        |> processKM km
        |> processChapters km.entities.chapters
        |> processQuestions km.entities.questions
        |> processAnswers km.entities.answers


getParent : ParentMap -> String -> String
getParent parentMap uuid =
    Maybe.withDefault nilUuid <| Dict.get uuid parentMap


filterWithTags : List String -> KnowledgeModel -> KnowledgeModel
filterWithTags tags km =
    let
        questions =
            Dict.filter (\k _ -> List.member k tags) km.entities.questions
    in
    { km | entities = KnowledgeModelEntities.updateQuestions questions km.entities }
