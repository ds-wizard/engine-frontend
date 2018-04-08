module KnowledgeModels.Editor.Models.Entities exposing (..)

import Json.Decode as Decode exposing (..)
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
    , shortUuid : Maybe String
    , text : String
    , itemName : String
    , answers : List Answer
    , references : List Reference
    , experts : List Expert
    }


type alias Answer =
    { uuid : String
    , label : String
    , advice : Maybe String
    , followUps : FollowUps
    }


type FollowUps
    = FollowUps (List Question)


type alias Reference =
    { uuid : String
    , chapter : String
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
        |> required "shortUuid" (Decode.nullable Decode.string)
        |> required "text" Decode.string
        |> optional "itemName" Decode.string "Item"
        |> required "answers" (Decode.lazy (\_ -> Decode.list answerDecoder))
        |> required "references" (Decode.list referenceDecoder)
        |> required "experts" (Decode.list expertDecoder)


answerDecoder : Decoder Answer
answerDecoder =
    decode Answer
        |> required "uuid" Decode.string
        |> required "label" Decode.string
        |> required "advice" (Decode.nullable Decode.string)
        |> required "followUps" followupsDecoder


followupsDecoder : Decoder FollowUps
followupsDecoder =
    Decode.map FollowUps (Decode.lazy (\_ -> Decode.list questionDecoder))


referenceDecoder : Decoder Reference
referenceDecoder =
    decode Reference
        |> required "uuid" Decode.string
        |> required "chapter" Decode.string


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
    , type_ = ""
    , title = "New question"
    , shortUuid = Nothing
    , text = "Question text"
    , itemName = "Item"
    , answers = []
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
    { uuid = uuid
    , chapter = "New reference"
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
            List.map getFollowUpQuestions question.answers
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
        |> List.map .answers
        |> List.concat


getAnswer : KnowledgeModel -> String -> Maybe Answer
getAnswer km answerUuid =
    getAnswers km
        |> List.find (\a -> a.uuid == answerUuid)


getReferences : KnowledgeModel -> List Reference
getReferences km =
    getQuestions km
        |> List.map .references
        |> List.concat


getReference : KnowledgeModel -> String -> Maybe Reference
getReference km referenceUuid =
    getReferences km
        |> List.find (\r -> r.uuid == referenceUuid)


getExperts : KnowledgeModel -> List Expert
getExperts km =
    getQuestions km
        |> List.map .experts
        |> List.concat


getExpert : KnowledgeModel -> String -> Maybe Expert
getExpert km expertUuid =
    getExperts km
        |> List.find (\e -> e.uuid == expertUuid)
