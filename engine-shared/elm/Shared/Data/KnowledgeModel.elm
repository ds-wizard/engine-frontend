module Shared.Data.KnowledgeModel exposing
    ( KnowledgeModel
    , ParentMap
    , createParentMap
    , decoder
    , filterWithTags
    , getAllQuestions
    , getAnswer
    , getAnswerFollowupQuestions
    , getChapter
    , getChapterQuestions
    , getChapters
    , getChoice
    , getExpert
    , getIntegration
    , getIntegrations
    , getParent
    , getQuestion
    , getQuestionAnswers
    , getQuestionChoices
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
import Maybe.Extra as Maybe
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Expert exposing (Expert)
import Shared.Data.KnowledgeModel.Integration exposing (Integration)
import Shared.Data.KnowledgeModel.KnowledgeModelEntities as KnowledgeModelEntities exposing (KnowledgeModelEntities)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)
import Shared.Data.KnowledgeModel.Reference exposing (Reference)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Utils exposing (nilUuid)
import Uuid exposing (Uuid)


type alias KnowledgeModel =
    { uuid : Uuid
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
        |> D.required "uuid" Uuid.decoder
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


getChoice : String -> KnowledgeModel -> Maybe Choice
getChoice uuid km =
    Dict.get uuid km.entities.choices


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
    getEntities .questions Question.getItemQuestionUuids .questions


getAnswerFollowupQuestions : String -> KnowledgeModel -> List Question
getAnswerFollowupQuestions =
    getEntities .answers .followUpUuids .questions


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
    if List.isEmpty tags then
        km

    else
        let
            filter _ question =
                Question.getTagUuids question
                    |> List.any (\t -> List.member t tags)

            questions =
                Dict.filter filter km.entities.questions
        in
        { km | entities = KnowledgeModelEntities.updateQuestions questions km.entities }
