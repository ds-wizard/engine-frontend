module KMEditor.Common.Models.Entities exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import List.Extra as List


type alias KnowledgeModel =
    { uuid : String
    , name : String
    , chapters : List Chapter
    }


type alias Chapter =
    { uuid : String
    , title : String
    , text : String
    , questions : List Question
    }


type alias Question =
    { uuid : String
    , type_ : String
    , title : String
    , text : String
    , answerItemTemplate : Maybe AnswerItemTemplate
    , answers : Maybe (List Answer)
    , references : List Reference
    , experts : List Expert
    }


type alias AnswerItemTemplate =
    { title : String
    , questions : AnswerItemTemplateQuestions
    }


type AnswerItemTemplateQuestions
    = AnswerItemTemplateQuestions (List Question)


type alias Answer =
    { uuid : String
    , label : String
    , advice : Maybe String
    , followUps : FollowUps
    }


type FollowUps
    = FollowUps (List Question)


type Reference
    = ResourcePageReference ResourcePageReferenceData
    | URLReference URLReferenceData
    | CrossReference CrossReferenceData


type alias ResourcePageReferenceData =
    { uuid : String
    , shortUuid : String
    }


type alias URLReferenceData =
    { uuid : String
    , url : String
    , anchor : String
    }


type alias CrossReferenceData =
    { uuid : String
    , targetUuid : String
    , description : String
    }


type alias Expert =
    { uuid : String
    , name : String
    , email : String
    }


knowledgeModelDecoder : Decoder KnowledgeModel
knowledgeModelDecoder =
    decode KnowledgeModel
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "chapters" (Decode.list chapterDecoder)


chapterDecoder : Decoder Chapter
chapterDecoder =
    decode Chapter
        |> required "uuid" Decode.string
        |> required "title" Decode.string
        |> required "text" Decode.string
        |> required "questions" (Decode.list questionDecoder)


questionDecoder : Decoder Question
questionDecoder =
    decode Question
        |> required "uuid" Decode.string
        |> required "type" Decode.string
        |> required "title" Decode.string
        |> required "text" Decode.string
        |> required "answerItemTemplate" (Decode.nullable <| Decode.lazy (\_ -> answerItemTemplateDecoder))
        |> required "answers" (Decode.nullable <| Decode.lazy (\_ -> Decode.list answerDecoder))
        |> required "references" (Decode.list referenceDecoder)
        |> required "experts" (Decode.list expertDecoder)


answerItemTemplateDecoder : Decoder AnswerItemTemplate
answerItemTemplateDecoder =
    decode AnswerItemTemplate
        |> required "title" Decode.string
        |> required "questions" (Decode.lazy (\_ -> answerItemTemplateQuestionsDecoder))


answerItemTemplateQuestionsDecoder : Decoder AnswerItemTemplateQuestions
answerItemTemplateQuestionsDecoder =
    Decode.map AnswerItemTemplateQuestions (Decode.list questionDecoder)


answerDecoder : Decoder Answer
answerDecoder =
    decode Answer
        |> required "uuid" Decode.string
        |> required "label" Decode.string
        |> required "advice" (Decode.nullable Decode.string)
        |> required "followUps" (Decode.lazy (\_ -> followupsDecoder))


followupsDecoder : Decoder FollowUps
followupsDecoder =
    Decode.map FollowUps (Decode.list questionDecoder)


referenceDecoder : Decoder Reference
referenceDecoder =
    Decode.oneOf
        [ when referenceType ((==) "ResourcePageReference") resourcePageReferenceDecoder
        , when referenceType ((==) "URLReference") urlReferenceDecoder
        , when referenceType ((==) "CrossReference") crossReferenceDecoder
        ]


referenceType : Decoder String
referenceType =
    Decode.field "referenceType" Decode.string


resourcePageReferenceDecoder : Decoder Reference
resourcePageReferenceDecoder =
    decode ResourcePageReferenceData
        |> required "uuid" Decode.string
        |> required "shortUuid" Decode.string
        |> Decode.map ResourcePageReference


urlReferenceDecoder : Decoder Reference
urlReferenceDecoder =
    decode URLReferenceData
        |> required "uuid" Decode.string
        |> required "url" Decode.string
        |> required "anchor" Decode.string
        |> Decode.map URLReference


crossReferenceDecoder : Decoder Reference
crossReferenceDecoder =
    decode CrossReferenceData
        |> required "uuid" Decode.string
        |> required "targetUuid" Decode.string
        |> required "description" Decode.string
        |> Decode.map CrossReference


expertDecoder : Decoder Expert
expertDecoder =
    decode Expert
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "email" Decode.string


newChapter : String -> Chapter
newChapter uuid =
    { uuid = uuid
    , title = "New chapter"
    , text = "Chapter text"
    , questions = []
    }


newQuestion : String -> Question
newQuestion uuid =
    { uuid = uuid
    , type_ = "options"
    , title = "New question"
    , text = "Question text"
    , answerItemTemplate = Nothing
    , answers = Nothing
    , references = []
    , experts = []
    }


newAnswer : String -> Answer
newAnswer uuid =
    { uuid = uuid
    , label = "New answer"
    , advice = Nothing
    , followUps = FollowUps []
    }


newReference : String -> Reference
newReference uuid =
    URLReference
        { uuid = uuid
        , url = "http://example.com"
        , anchor = "See also"
        }


newExpert : String -> Expert
newExpert uuid =
    { uuid = uuid
    , name = "New expert"
    , email = "expert@example.com"
    }


getChapters : KnowledgeModel -> List Chapter
getChapters km =
    km.chapters


getChapter : KnowledgeModel -> String -> Maybe Chapter
getChapter km chapterUuid =
    getChapters km
        |> List.find (\c -> c.uuid == chapterUuid)


getQuestions : KnowledgeModel -> List Question
getQuestions km =
    let
        nestedQuestions question =
            List.map getFollowUpQuestions (question.answers |> Maybe.withDefault [])
                |> List.concat
                |> (::) question
    in
    getChapters km
        |> List.map .questions
        |> List.concat
        |> List.map nestedQuestions
        |> List.concat


getQuestion : KnowledgeModel -> String -> Maybe Question
getQuestion km questionUuid =
    getQuestions km
        |> List.find (\q -> q.uuid == questionUuid)


getAnswerItemTemplateQuestions : Question -> List Question
getAnswerItemTemplateQuestions question =
    let
        unwrap (AnswerItemTemplateQuestions questions) =
            questions
    in
    question.answerItemTemplate
        |> Maybe.map (.questions >> unwrap)
        |> Maybe.withDefault []


getFollowUpQuestions : Answer -> List Question
getFollowUpQuestions answer =
    let
        getFollowUpsQuestionList (FollowUps questions) =
            questions
    in
    getFollowUpsQuestionList answer.followUps


getAnswers : KnowledgeModel -> List Answer
getAnswers km =
    getQuestions km
        |> List.map (.answers >> Maybe.withDefault [])
        |> List.concat


getAnswer : KnowledgeModel -> String -> Maybe Answer
getAnswer km answerUuid =
    getAnswers km
        |> List.find (\a -> a.uuid == answerUuid)


getReferenceUuid : Reference -> String
getReferenceUuid =
    referenceByType .uuid .uuid .uuid


getReferenceVisibleName : Reference -> String
getReferenceVisibleName =
    referenceByType .shortUuid .anchor .targetUuid


getReferences : KnowledgeModel -> List Reference
getReferences km =
    getQuestions km
        |> List.map .references
        |> List.concat


getReference : KnowledgeModel -> String -> Maybe Reference
getReference km referenceUuid =
    getReferences km
        |> List.find (\r -> getReferenceUuid r == referenceUuid)


getExperts : KnowledgeModel -> List Expert
getExperts km =
    getQuestions km
        |> List.map .experts
        |> List.concat


getExpert : KnowledgeModel -> String -> Maybe Expert
getExpert km expertUuid =
    getExperts km
        |> List.find (\e -> e.uuid == expertUuid)


referenceByType : (ResourcePageReferenceData -> a) -> (URLReferenceData -> a) -> (CrossReferenceData -> a) -> Reference -> a
referenceByType resourcePageReference urlReference crossReference reference =
    case reference of
        ResourcePageReference data ->
            resourcePageReference data

        URLReference data ->
            urlReference data

        CrossReference data ->
            crossReference data
