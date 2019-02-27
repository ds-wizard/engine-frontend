module KMEditor.Common.Models.Entities exposing
    ( Answer
    , Chapter
    , CrossReferenceData
    , Expert
    , FollowUps(..)
    , KnowledgeModel
    , Level
    , ListQuestionData
    , Metric
    , MetricMeasure
    , OptionsQuestionData
    , Question(..)
    , Reference(..)
    , ResourcePageReferenceData
    , Tag
    , URLReferenceData
    , ValueQuestionData
    , ValueQuestionType(..)
    , answerDecoder
    , chapterDecoder
    , createPathMap
    , expertDecoder
    , filterKnowledgModelWithTags
    , getAnswer
    , getAnswers
    , getChapter
    , getChapters
    , getExpert
    , getExperts
    , getFollowUpQuestions
    , getQuestion
    , getQuestionAnswers
    , getQuestionExperts
    , getQuestionItemQuestions
    , getQuestionItemTitle
    , getQuestionReferences
    , getQuestionRequiredLevel
    , getQuestionTagUuids
    , getQuestionText
    , getQuestionTitle
    , getQuestionTypeString
    , getQuestionUuid
    , getQuestionValueType
    , getQuestions
    , getReference
    , getReferenceUuid
    , getReferenceVisibleName
    , getReferences
    , getTag
    , isQuestionList
    , isQuestionOptions
    , knowledgeModelDecoder
    , levelListDecoder
    , mapReferenceData
    , metricDecoder
    , metricListDecoder
    , metricMeasureDecoder
    , metricMeasureEncoder
    , newAnswer
    , newChapter
    , newExpert
    , newQuestion
    , newReference
    , newTag
    , questionDecoder
    , referenceDecoder
    , tagDecoder
    , valueTypeDecoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (..)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (..)
import KMEditor.Common.Models.Path exposing (Path, PathNode(..))
import List.Extra as List


type alias KnowledgeModel =
    { uuid : String
    , name : String
    , chapters : List Chapter
    , tags : List Tag
    }


type alias Tag =
    { uuid : String
    , name : String
    , description : Maybe String
    , color : String
    }


type alias Chapter =
    { uuid : String
    , title : String
    , text : String
    , questions : List Question
    }


type Question
    = OptionsQuestion OptionsQuestionData
    | ListQuestion ListQuestionData
    | ValueQuestion ValueQuestionData


type alias OptionsQuestionData =
    { uuid : String
    , title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , tagUuids : List String
    , references : List Reference
    , experts : List Expert
    , answers : List Answer
    }


type alias ListQuestionData =
    { uuid : String
    , title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , tagUuids : List String
    , references : List Reference
    , experts : List Expert
    , itemTemplateTitle : String
    , itemTemplateQuestions : List Question
    }


type alias ValueQuestionData =
    { uuid : String
    , title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , tagUuids : List String
    , references : List Reference
    , experts : List Expert
    , valueType : ValueQuestionType
    }


type ValueQuestionType
    = StringValueType
    | DateValueType
    | NumberValueType
    | TextValueType


type alias Answer =
    { uuid : String
    , label : String
    , advice : Maybe String
    , metricMeasures : List MetricMeasure
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
    , label : String
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


type alias Metric =
    { uuid : String
    , title : String
    , abbreviation : String
    , description : String
    }


type alias MetricMeasure =
    { metricUuid : String
    , measure : Float
    , weight : Float
    }


type alias Level =
    { level : Int
    , title : String
    }



{- Decoders -}


knowledgeModelDecoder : Decoder KnowledgeModel
knowledgeModelDecoder =
    Decode.succeed KnowledgeModel
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "chapters" (Decode.list chapterDecoder)
        |> required "tags" (Decode.list tagDecoder)


chapterDecoder : Decoder Chapter
chapterDecoder =
    Decode.succeed Chapter
        |> required "uuid" Decode.string
        |> required "title" Decode.string
        |> required "text" Decode.string
        |> required "questions" (Decode.list questionDecoder)


tagDecoder : Decoder Tag
tagDecoder =
    Decode.succeed Tag
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "description" (Decode.nullable Decode.string)
        |> required "color" Decode.string


questionDecoder : Decoder Question
questionDecoder =
    Decode.oneOf
        [ when questionType ((==) "OptionsQuestion") optionsQuestionDecoder
        , when questionType ((==) "ListQuestion") listQuestionDecoder
        , when questionType ((==) "ValueQuestion") valueQuestionDecoder
        ]


questionType : Decoder String
questionType =
    Decode.field "questionType" Decode.string


optionsQuestionDecoder : Decoder Question
optionsQuestionDecoder =
    Decode.map OptionsQuestion optionsQuestionDataDecoder


listQuestionDecoder : Decoder Question
listQuestionDecoder =
    Decode.map ListQuestion listQuestionDataDecoder


valueQuestionDecoder : Decoder Question
valueQuestionDecoder =
    Decode.map ValueQuestion valueQuestionDataDecoder


optionsQuestionDataDecoder : Decoder OptionsQuestionData
optionsQuestionDataDecoder =
    Decode.succeed OptionsQuestionData
        |> required "uuid" Decode.string
        |> required "title" Decode.string
        |> required "text" (Decode.nullable Decode.string)
        |> required "requiredLevel" (Decode.nullable Decode.int)
        |> required "tagUuids" (Decode.list Decode.string)
        |> required "references" (Decode.list referenceDecoder)
        |> required "experts" (Decode.list expertDecoder)
        |> required "answers" (Decode.list answerDecoder)


listQuestionDataDecoder : Decoder ListQuestionData
listQuestionDataDecoder =
    Decode.succeed ListQuestionData
        |> required "uuid" Decode.string
        |> required "title" Decode.string
        |> required "text" (Decode.nullable Decode.string)
        |> required "requiredLevel" (Decode.nullable Decode.int)
        |> required "tagUuids" (Decode.list Decode.string)
        |> required "references" (Decode.list referenceDecoder)
        |> required "experts" (Decode.list expertDecoder)
        |> required "itemTemplateTitle" Decode.string
        |> required "itemTemplateQuestions" (Decode.lazy (\_ -> Decode.list questionDecoder))


valueQuestionDataDecoder : Decoder ValueQuestionData
valueQuestionDataDecoder =
    Decode.succeed ValueQuestionData
        |> required "uuid" Decode.string
        |> required "title" Decode.string
        |> required "text" (Decode.nullable Decode.string)
        |> required "requiredLevel" (Decode.nullable Decode.int)
        |> required "tagUuids" (Decode.list Decode.string)
        |> required "references" (Decode.list referenceDecoder)
        |> required "experts" (Decode.list expertDecoder)
        |> required "valueType" valueTypeDecoder


valueTypeDecoder : Decoder ValueQuestionType
valueTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "StringValue" ->
                        Decode.succeed StringValueType

                    "DateValue" ->
                        Decode.succeed DateValueType

                    "NumberValue" ->
                        Decode.succeed NumberValueType

                    "TextValue" ->
                        Decode.succeed TextValueType

                    valueType ->
                        Decode.fail <| "Unknown value type: " ++ valueType
            )


answerDecoder : Decoder Answer
answerDecoder =
    Decode.succeed Answer
        |> required "uuid" Decode.string
        |> required "label" Decode.string
        |> required "advice" (Decode.nullable Decode.string)
        |> required "metricMeasures" (Decode.list metricMeasureDecoder)
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
    Decode.succeed ResourcePageReferenceData
        |> required "uuid" Decode.string
        |> required "shortUuid" Decode.string
        |> Decode.map ResourcePageReference


urlReferenceDecoder : Decoder Reference
urlReferenceDecoder =
    Decode.succeed URLReferenceData
        |> required "uuid" Decode.string
        |> required "url" Decode.string
        |> required "label" Decode.string
        |> Decode.map URLReference


crossReferenceDecoder : Decoder Reference
crossReferenceDecoder =
    Decode.succeed CrossReferenceData
        |> required "uuid" Decode.string
        |> required "targetUuid" Decode.string
        |> required "description" Decode.string
        |> Decode.map CrossReference


expertDecoder : Decoder Expert
expertDecoder =
    Decode.succeed Expert
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "email" Decode.string


metricDecoder : Decoder Metric
metricDecoder =
    Decode.succeed Metric
        |> required "uuid" Decode.string
        |> required "title" Decode.string
        |> required "abbreviation" Decode.string
        |> required "description" Decode.string


metricListDecoder : Decoder (List Metric)
metricListDecoder =
    Decode.list metricDecoder


metricMeasureDecoder : Decoder MetricMeasure
metricMeasureDecoder =
    Decode.succeed MetricMeasure
        |> required "metricUuid" Decode.string
        |> required "measure" Decode.float
        |> required "weight" Decode.float


metricMeasureEncoder : MetricMeasure -> Encode.Value
metricMeasureEncoder metricMeasure =
    Encode.object
        [ ( "metricUuid", Encode.string metricMeasure.metricUuid )
        , ( "measure", Encode.float metricMeasure.measure )
        , ( "weight", Encode.float metricMeasure.weight )
        ]


levelDecoder : Decoder Level
levelDecoder =
    Decode.succeed Level
        |> required "level" Decode.int
        |> required "title" Decode.string


levelListDecoder : Decoder (List Level)
levelListDecoder =
    Decode.list levelDecoder



{- New entities -}


newChapter : String -> Chapter
newChapter uuid =
    { uuid = uuid
    , title = "New chapter"
    , text = "Chapter text"
    , questions = []
    }


newTag : String -> Tag
newTag uuid =
    { uuid = uuid
    , name = "New Tag"
    , description = Nothing
    , color = "#3498DB"
    }


newQuestion : String -> Question
newQuestion uuid =
    OptionsQuestion
        { uuid = uuid
        , title = "New question"
        , text = Nothing
        , requiredLevel = Nothing
        , tagUuids = []
        , references = []
        , experts = []
        , answers = []
        }


newAnswer : String -> Answer
newAnswer uuid =
    { uuid = uuid
    , label = "New answer"
    , advice = Nothing
    , followUps = FollowUps []
    , metricMeasures = []
    }


newReference : String -> Reference
newReference uuid =
    URLReference
        { uuid = uuid
        , url = "http://example.com"
        , label = "See also"
        }


newExpert : String -> Expert
newExpert uuid =
    { uuid = uuid
    , name = "New expert"
    , email = "expert@example.com"
    }



{- Helpers -}


createPathMap : KnowledgeModel -> Dict String Path
createPathMap knowledgeModel =
    let
        foldKm path km dict =
            let
                nextPath =
                    path ++ [ KMPathNode km.uuid ]

                withChapters =
                    List.foldl (foldChapter nextPath) dict km.chapters

                withTags =
                    List.foldl (foldTag nextPath) withChapters km.tags
            in
            Dict.insert km.uuid path withTags

        foldChapter path chapter dict =
            let
                nextPath =
                    path ++ [ ChapterPathNode chapter.uuid ]

                withQuestions =
                    List.foldl (foldQuestion nextPath) dict chapter.questions
            in
            Dict.insert chapter.uuid path withQuestions

        foldTag path tag dict =
            Dict.insert tag.uuid path dict

        foldQuestion path question dict =
            let
                nextPath =
                    path ++ [ QuestionPathNode (getQuestionUuid question) ]

                withAnswers =
                    List.foldl (foldAnswer nextPath) dict <| getQuestionAnswers question

                withItemQuestions =
                    List.foldl (foldQuestion nextPath) withAnswers <| getQuestionItemQuestions question

                withReferences =
                    List.foldl (foldReference nextPath) withItemQuestions <| getQuestionReferences question

                withExperts =
                    List.foldl (foldExpert nextPath) withReferences <| getQuestionExperts question
            in
            Dict.insert (getQuestionUuid question) path withExperts

        foldAnswer path answer dict =
            let
                nextPath =
                    path ++ [ AnswerPathNode answer.uuid ]

                withFollowUps =
                    List.foldl (foldQuestion nextPath) dict <| getFollowUpQuestions answer
            in
            Dict.insert answer.uuid path withFollowUps

        foldExpert path expert dict =
            Dict.insert expert.uuid path dict

        foldReference path reference dict =
            Dict.insert (getReferenceUuid reference) path dict
    in
    foldKm [] knowledgeModel Dict.empty


getChapters : KnowledgeModel -> List Chapter
getChapters km =
    km.chapters


getChapter : KnowledgeModel -> String -> Maybe Chapter
getChapter km chapterUuid =
    getChapters km
        |> List.find (\c -> c.uuid == chapterUuid)


getTag : KnowledgeModel -> String -> Maybe Tag
getTag km tagUuid =
    List.find (\t -> t.uuid == tagUuid) km.tags


getQuestions : KnowledgeModel -> List Question
getQuestions km =
    let
        foldAnswerQuestions answer =
            List.foldl (\q acc -> acc ++ foldQuestion q) [] (getFollowUpQuestions answer)

        foldQuestion question =
            case question of
                OptionsQuestion questionData ->
                    [ question ] ++ List.foldl (\a acc -> acc ++ foldAnswerQuestions a) [] questionData.answers

                ListQuestion questionData ->
                    [ question ] ++ List.foldl (\q acc -> acc ++ foldQuestion q) [] questionData.itemTemplateQuestions

                ValueQuestion _ ->
                    [ question ]

        foldChapter chapter =
            List.foldl (\q acc -> acc ++ foldQuestion q) [] chapter.questions
    in
    List.foldl (\c acc -> acc ++ foldChapter c) [] km.chapters


getQuestion : KnowledgeModel -> String -> Maybe Question
getQuestion km questionUuid =
    getQuestions km
        |> List.find (\q -> getQuestionUuid q == questionUuid)


mapQuestionData : (OptionsQuestionData -> a) -> (ListQuestionData -> a) -> (ValueQuestionData -> a) -> Question -> a
mapQuestionData fn1 fn2 fn3 question =
    case question of
        OptionsQuestion data ->
            fn1 data

        ListQuestion data ->
            fn2 data

        ValueQuestion data ->
            fn3 data


getQuestionUuid : Question -> String
getQuestionUuid =
    mapQuestionData .uuid .uuid .uuid


getQuestionTitle : Question -> String
getQuestionTitle =
    mapQuestionData .title .title .title


getQuestionText : Question -> Maybe String
getQuestionText =
    mapQuestionData .text .text .text


getQuestionRequiredLevel : Question -> Maybe Int
getQuestionRequiredLevel =
    mapQuestionData .requiredLevel .requiredLevel .requiredLevel


getQuestionTagUuids : Question -> List String
getQuestionTagUuids =
    mapQuestionData .tagUuids .tagUuids .tagUuids


getQuestionExperts : Question -> List Expert
getQuestionExperts =
    mapQuestionData .experts .experts .experts


getQuestionReferences : Question -> List Reference
getQuestionReferences =
    mapQuestionData .references .references .references


getQuestionAnswers : Question -> List Answer
getQuestionAnswers question =
    case question of
        OptionsQuestion optionsQuestionData ->
            optionsQuestionData.answers

        _ ->
            []


getQuestionItemTitle : Question -> Maybe String
getQuestionItemTitle question =
    case question of
        ListQuestion listQuestionData ->
            Just listQuestionData.itemTemplateTitle

        _ ->
            Nothing


getQuestionItemQuestions : Question -> List Question
getQuestionItemQuestions question =
    case question of
        ListQuestion listQuestionData ->
            listQuestionData.itemTemplateQuestions

        _ ->
            []


getQuestionValueType : Question -> Maybe ValueQuestionType
getQuestionValueType question =
    case question of
        ValueQuestion valueQuestionData ->
            Just valueQuestionData.valueType

        _ ->
            Nothing


getQuestionTypeString : Question -> String
getQuestionTypeString =
    mapQuestionData
        (\_ -> "Options")
        (\_ -> "List")
        (\_ -> "Value")


isQuestionOptions : Question -> Bool
isQuestionOptions question =
    case question of
        OptionsQuestion _ ->
            True

        _ ->
            False


isQuestionList : Question -> Bool
isQuestionList question =
    case question of
        ListQuestion _ ->
            True

        _ ->
            False


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
        |> List.map getQuestionAnswers
        |> List.concat


getAnswer : KnowledgeModel -> String -> Maybe Answer
getAnswer km answerUuid =
    getAnswers km
        |> List.find (\a -> a.uuid == answerUuid)


getReferenceUuid : Reference -> String
getReferenceUuid =
    mapReferenceData .uuid .uuid .uuid


getReferenceVisibleName : Reference -> String
getReferenceVisibleName =
    mapReferenceData .shortUuid .label .targetUuid


getReferences : KnowledgeModel -> List Reference
getReferences km =
    getQuestions km
        |> List.map getQuestionReferences
        |> List.concat


getReference : KnowledgeModel -> String -> Maybe Reference
getReference km referenceUuid =
    getReferences km
        |> List.find (\r -> getReferenceUuid r == referenceUuid)


getExperts : KnowledgeModel -> List Expert
getExperts km =
    getQuestions km
        |> List.map getQuestionExperts
        |> List.concat


getExpert : KnowledgeModel -> String -> Maybe Expert
getExpert km expertUuid =
    getExperts km
        |> List.find (\e -> e.uuid == expertUuid)


mapReferenceData : (ResourcePageReferenceData -> a) -> (URLReferenceData -> a) -> (CrossReferenceData -> a) -> Reference -> a
mapReferenceData resourcePageReference urlReference crossReference reference =
    case reference of
        ResourcePageReference data ->
            resourcePageReference data

        URLReference data ->
            urlReference data

        CrossReference data ->
            crossReference data


filterKnowledgModelWithTags : List String -> KnowledgeModel -> KnowledgeModel
filterKnowledgModelWithTags selectedTags originalKM =
    let
        mapKM tags km =
            { km
                | chapters =
                    km.chapters
                        |> List.map (mapChapter tags)
                        |> List.filter filterEmptyChapter
            }

        filterEmptyChapter chapter =
            List.length chapter.questions > 0

        mapChapter tags chapter =
            { chapter
                | questions =
                    chapter.questions
                        |> List.filter (filterQuestion tags)
                        |> List.map (mapQuestion tags)
            }

        filterQuestion tags question =
            getQuestionTagUuids question |> List.any (\t -> List.member t tags)

        mapQuestion tags question =
            case question of
                OptionsQuestion data ->
                    OptionsQuestion
                        { data | answers = List.map (mapAnswer tags) data.answers }

                ListQuestion data ->
                    ListQuestion
                        { data
                            | itemTemplateQuestions =
                                data.itemTemplateQuestions
                                    |> List.filter (filterQuestion tags)
                                    |> List.map (mapQuestion tags)
                        }

                _ ->
                    question

        mapAnswer tags answer =
            { answer
                | followUps =
                    getFollowUpQuestions answer
                        |> List.filter (filterQuestion tags)
                        |> List.map (mapQuestion tags)
                        |> FollowUps
            }
    in
    if List.isEmpty selectedTags then
        originalKM

    else
        mapKM selectedTags originalKM
